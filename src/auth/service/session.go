package service

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/access_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/session_repository"
)

// Params
const (
	maxUniqueSessionTime = time.Hour * 24 * 7
	maxAccessSessionTime = time.Hour * 3
)

type TokenGenerator interface {
	NewSessionToken(expiredAt time.Time, idUser int64) (string, error)
	NewAccessToken(expiredAt time.Time, user model.User) (string, error)
	NewFirstTimeToken(IDUser int64) (string, error)
}

type sessionService struct {
	sessionRepository session_repository.SessionRepository
	accessRepository  access_repository.AccessRepository
	tokenGenerator    TokenGenerator
}

func (sessionService *sessionService) NewSession(
	sessionDto dto.SessionDto,
	idAuth,
	idUser int64,
) (string, error) {
	expiredAt := time.Now().Add(maxUniqueSessionTime)
	token, err := sessionService.tokenGenerator.NewSessionToken(
		expiredAt,
		idUser,
	)
	if err != nil {
		return "", err
	}

	session := model.Session{
		Token:     token,
		Device:    sessionDto.Device,
		IP:        sessionDto.IP,
		Location:  sessionDto.Location,
		Browser:   sessionDto.Browser,
		ExpiresAt: expiredAt,
		IDAuth:    idAuth,
	}

	_, err = sessionService.sessionRepository.InsertOne(session)
	if err != nil {
		return "", err
	}

	return token, nil
}

func (sessionService *sessionService) RefreshSession(
	sessionToken string,
	idUser int64,
) (string, error) {
	expiredAt := time.Now().Add(maxUniqueSessionTime)
	token, err := sessionService.tokenGenerator.NewSessionToken(
		expiredAt,
		idUser,
	)
	if err != nil {
		return "", err
	}

	return token, sessionService.sessionRepository.Update(
		&session_repository.SessionCriteria{
			Token: sessionToken,
		},
		session_repository.SessionUpdateData{
			Token:     token,
			ExpiresAt: &expiredAt,
		},
	)
}

func (sessionService *sessionService) GenerateAccess(
	sessionToken string,
	user *model.User,
) (string, error) {
	now := time.Now()
	idSession, err := sessionService.sessionRepository.Exists(
		&session_repository.SessionCriteria{
			Token:       sessionToken,
			ExpiredAtGt: &now,
		},
	)
	if err != nil {
		return "", err
	}
	if idSession == 0 {
		return "", ErrSessionNotExists
	}

	token, err := sessionService.tokenGenerator.NewAccessToken(
		time.Now().Add(maxAccessSessionTime),
		*user,
	)
	if err != nil {
		return "", err
	}
	// Insert access
	access := model.Access{
		Token:     token,
		ExpiresAt: time.Now(),
		IDAccess:  idSession,
	}
	idAccess, err := sessionService.accessRepository.InsertOne(access)
	if err != nil {
		return "", err
	}
	// Revoke all tokens before new token
	go func() {
		time.Sleep(5 * time.Second)
		var currentRevoked = false
		var toUpdateRevoked = true

		err = sessionService.accessRepository.Update(
			&access_repository.AccessCriteria{
				IsRevoked: &currentRevoked,
				ID_NE:     idAccess,
			},
			access_repository.AccessUpdateData{
				IsRevoked: &toUpdateRevoked,
			},
		)
	}()

	return token, err
}

func (sessionService *sessionService) CheckToken(token string) error {
	isNotRevoked := false

	existsToken, err := sessionService.accessRepository.Exists(
		&access_repository.AccessCriteria{
			Token:     token,
			IsRevoked: &isNotRevoked,
		},
	)
	if err != nil {
		return err
	}
	if existsToken == 0 {
		return ErrTokenRevokedOrNotExists
	}
	return nil
}

func (sessionService *sessionService) DeleteRevokedTokens() error {
	isRevoked := true

	return sessionService.accessRepository.Delete(
		&access_repository.AccessCriteria{
			IsRevoked: &isRevoked,
		},
	)
}

func (sessionService *sessionService) DeleteExpiredSessions() error {
	expired := time.Now()

	return sessionService.sessionRepository.Delete(
		&session_repository.SessionCriteria{
			ExpiredAtLt: &expired,
		},
	)
}

func NewSessionService(
	sessionRepository session_repository.SessionRepository,
	accessRepository access_repository.AccessRepository,
	tokenGenerator TokenGenerator,
) *sessionService {
	return &sessionService{
		sessionRepository: sessionRepository,
		accessRepository:  accessRepository,
		tokenGenerator:    tokenGenerator,
	}
}

package queue

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/go-playground/validator/v10"
	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
)

const NATS_QUEUE = "auth"

type NatsClient struct {
	conn      *nats.Conn
	js        jetstream.JetStream
	jsContext nats.JetStreamContext
	streams   map[string]jetstream.Stream
	logger    logger.Logger
	validate  *validator.Validate
}

var settingsData = settings.GetSettings()

func newConnectionNatsCore() *nats.Conn {
	natsHosts := strings.Split(settingsData.NATS_HOSTS, ",")
	var natsServers []string
	for _, natsHost := range natsHosts {
		uriNats := fmt.Sprintf("nats://%s", natsHost)
		natsServers = append(natsServers, uriNats)
	}
	nc, err := nats.Connect(strings.Join(natsServers, ","))
	if err != nil {
		panic(err)
	}
	return nc
}

func (natsClient *NatsClient) addStreams() {
	natsClient.streams = make(map[string]jetstream.Stream)
	streams := []string{"DOGS"}
	// Exists stream
	contextList, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	streamsList := natsClient.js.ListStreams(contextList)
	var infoStreams []string
	for s := range streamsList.Info() {
		infoStreams = append(infoStreams, s.Config.Name)
	}

	for _, expectedStream := range streams {
		var exists bool
		for _, s := range infoStreams {
			if expectedStream == s {
				exists = true
			}
		}
		if !exists {
			if settingsData.GO_ENV == "prod" {
				log.Panicf("Stream %s not exists", expectedStream)
			} else {
				natsClient.js.CreateStream(context.Background(), jetstream.StreamConfig{
					Name:     expectedStream,
					Subjects: []string{strings.ToLower(expectedStream) + ".*"},
					Storage:  jetstream.MemoryStorage,
				})
			}
		}

		stream, err := natsClient.js.Stream(context.Background(), expectedStream)
		if err != nil {
			panic(err)
		}

		natsClient.streams[expectedStream] = stream
	}
}

func (natsClient *NatsClient) GetStream(stream string) jetstream.Stream {
	return natsClient.streams[stream]
}

func (natsClient *NatsClient) Publish(
	event bus.Event,
) error {
	_, err := natsClient.js.Publish(context.Background(), string(event.Name), event.Payload)
	return err
}

func (natsClient *NatsClient) Request(
	event bus.Event,
	toBind interface{},
) error {
	response, err := natsClient.conn.Request(string(event.Name), event.Payload, time.Minute*5)
	if err != nil {
		return err
	}
	// Handle res
	var natsResponse *DefaultNatsResponse[interface{}]

	if err := json.Unmarshal(response.Data, &natsResponse); err != nil {
		return errors.New("server error")
	}
	if !natsResponse.Success {
		return errors.New(natsResponse.Message)
	}
	// Decode and decode into struct
	dataBytes, err := json.Marshal(natsResponse.Data)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(dataBytes, toBind); err != nil {
		return err
	}

	return nil
}

func (natsClient *NatsClient) QueueSubscribe(
	subject string,
	cb func(msg *nats.Msg) *DefaultNatsResponse[interface{}],
) {
	_, err := natsClient.conn.QueueSubscribe(subject, NATS_QUEUE, func(msg *nats.Msg) {
		res := cb(msg)
		// Convert
		resBytes, _ := json.Marshal(res)
		msg.Respond(resBytes)
	})
	if err != nil {
		panic(err)
	}
}

func (natsClient *NatsClient) SubscribeAndRespond(
	name bus.EventName,
	handler func(c bus.Context) (*bus.BusResponse, error),
) {
	_, err := natsClient.conn.QueueSubscribe(string(name), NATS_QUEUE, func(msg *nats.Msg) {
		res, err := handler(bus.Context{
			Data: msg.Data,
			BindData: func(toBind interface{}) error {
				if err := json.Unmarshal(msg.Data, &toBind); err != nil {
					return err
				}
				return natsClient.validate.Struct(toBind)
			},
			EventTrigger: msg.Sub.Subject,
			Kill: func(reason string) error {
				natsClient.logger.Error(
					fmt.Sprintf("NATS TERM queue: %s: %s", string(name), reason),
				)

				resBytes, _ := json.Marshal(bus.BusResponse{
					Message: reason,
					Success: false,
				})
				return msg.Respond(resBytes)
			},
			FollowUp: func(delay time.Duration) error {
				natsClient.logger.Error(
					fmt.Sprintf("NATS TERM queue: %s: %s", string(name), "Cant follow up response"),
				)

				return nil
			},
		})
		if err != nil {
			resBytes, _ := json.Marshal(bus.BusResponse{
				Message: err.Error(),
				Success: false,
			})
			msg.Respond(resBytes)
			return
		}
		if res != nil {
			// Convert
			resBytes, _ := json.Marshal(res)
			msg.Respond(resBytes)
		}
	})
	if err != nil {
		panic(err)
	}
}

func (natsClient *NatsClient) Subscribe(
	name bus.EventName,
	handler func(c bus.Context) error,
) {
	splitName := strings.Split(string(name), ".")

	streamName := strings.ToUpper(splitName[0])
	stream := natsClient.GetStream(streamName)
	durable := strings.ReplaceAll(strings.ToUpper(splitName[1]), " ", "_")

	if stream == nil {
		panic(fmt.Sprintf("Stream %s not found", streamName))
	}
	cons, err := stream.CreateOrUpdateConsumer(context.Background(), jetstream.ConsumerConfig{
		Durable:        durable,
		FilterSubjects: []string{string(name)},
		AckPolicy:      jetstream.AckExplicitPolicy,
	})
	if err != nil {
		panic(err)
	}
	// Stream info
	info, err := stream.Info(context.Background())
	if err != nil {
		panic(err)
	}

	go func() {
		iter, err := cons.Messages(jetstream.PullMaxMessages(1))
		if err != nil {
			panic(err)
		}
		numWorkers := 5
		sem := make(chan struct{}, numWorkers)
		for {
			sem <- struct{}{}
			go func() {
				defer func() {
					<-sem
				}()
				msg, err := iter.Next()
				if err != nil {
					return
				}
				err = msg.InProgress()
				if err != nil {
					return
				}
				// TODO
				err = handler(bus.Context{
					EventTrigger: msg.Subject(),
					Kill: func(reason string) error {
						natsClient.logger.Error(
							fmt.Sprintf("NATS TERM consumer: %s [%s]: %s", string(name), info.Config.Name, reason),
						)
						return msg.Term()
					},
					FollowUp: func(delay time.Duration) error {
						return msg.NakWithDelay(delay)
					},
					Data: msg.Data(),
					BindData: func(toBind interface{}) error {
						if err := json.Unmarshal(msg.Data(), &toBind); err != nil {
							return err
						}
						return natsClient.validate.Struct(toBind)
					},
				})
				if err == nil {
					msg.Ack()
				} else {
					msg.Nak()
					natsClient.logger.Error(
						fmt.Sprintf("NATS Error consumer: %s [%s]: %s", string(name), info.Config.Name, err.Error()),
					)
				}
			}()
		}
	}()
}

func New(logger logger.Logger) bus.Bus {
	conn := newConnectionNatsCore()
	// Connect to JetStream
	js, err := jetstream.New(conn)
	if err != nil {
		panic(err)
	}

	jsContext, err := conn.JetStream()
	if err != nil {
		panic(err)
	}

	validate := validator.New(
		validator.WithRequiredStructEnabled(),
	)
	validate.SetTagName("binding")

	natsClient := &NatsClient{
		conn:      conn,
		js:        js,
		jsContext: jsContext,
		logger:    logger,
		validate:  validate,
	}
	// Add streams
	natsClient.addStreams()

	return natsClient
}

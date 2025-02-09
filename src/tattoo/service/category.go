package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/category_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var categoryService *CategoryService

type CategoryService struct {
	categoryRepository category_repository.CategoryRepository
}

func (categoryService *CategoryService) GetCategories() ([]model.Category, error) {
	return categoryService.categoryRepository.Find(&category_repository.Criteria{
		State: utils.Bool(true),
	})
}

func (categoryService *CategoryService) ExistsCategories(idCategories []int64) error {
	anyNotExists, err := utils.AnyMatch(idCategories, func(idCategory int64) (bool, error) {
		exists, err := categoryService.categoryRepository.Exists(&category_repository.Criteria{
			ID: idCategory,
		})
		if err != nil {
			return false, err
		}
		return !exists, nil
	})
	if err != nil {
		return err
	}
	if anyNotExists {
		return ErrCategoriesNotExists
	}

	return nil
}

func NewCategoryService(
	categoryRepository category_repository.CategoryRepository,
) *CategoryService {
	if categoryService == nil {
		categoryService = &CategoryService{
			categoryRepository: categoryRepository,
		}
	}

	return categoryService
}

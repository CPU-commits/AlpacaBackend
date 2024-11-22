package utils

import (
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

var Bundle *i18n.Bundle
var localizers map[string]*i18n.Localizer

func GetLocalizer(tag string) *i18n.Localizer {
	if localizers == nil {
		localizers = make(map[string]*i18n.Localizer)
		for _, tag := range Bundle.LanguageTags() {
			localizers[tag.String()] = i18n.NewLocalizer(Bundle, tag.String())
		}
	}

	localizer, ok := localizers[tag]
	if !ok {
		return localizers["es"]
	}
	return localizer
}

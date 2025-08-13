package utils

type ChangeFreq string

const (
	ALWAYS  ChangeFreq = "always"
	DAILY   ChangeFreq = "daily"
	HOURLY  ChangeFreq = "hourly"
	MONTHLY ChangeFreq = "monthly"
	NEVER   ChangeFreq = "never"
	WEEKLY  ChangeFreq = "weekly"
	YEARLY  ChangeFreq = "yearly"
)

type SitemapImage struct {
	Loc string `json:"loc"`
}

type SitemapXML struct {
	ChangeFreq ChangeFreq     `json:"changefreq"`
	Images     []SitemapImage `json:"images,omitempty"`
	LastMod    string         `json:"lastmod"`
	Priority   float32        `json:"priority"`
	Data       map[string]any `json:"data,omitempty"`
}

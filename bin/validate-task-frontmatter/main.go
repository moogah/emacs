package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"gopkg.in/yaml.v3"
)

type TaskFrontmatter struct {
	Name        string   `yaml:"name"`
	Description string   `yaml:"description"`
	Change      string   `yaml:"change"`
	Status      string   `yaml:"status"`
	Relations   []string `yaml:"relations,omitempty"`
}

var (
	validStatuses = map[string]bool{
		"blocked":      true,
		"ready":        true,
		"needs-review": true,
		"done":         true,
	}
	validRelationLabels = map[string]bool{
		"blocked-by":      true,
		"discovered-from": true,
		"enables":         true,
	}
	kebabCase = regexp.MustCompile(`^[a-z0-9]+(-[a-z0-9]+)*$`)
)

func extractFrontmatter(content string) (string, error) {
	content = strings.TrimLeft(content, "\ufeff")
	if !strings.HasPrefix(content, "---\n") && !strings.HasPrefix(content, "---\r\n") {
		return "", fmt.Errorf("file must start with a YAML frontmatter block delimited by '---'")
	}
	rest := strings.TrimPrefix(content, "---\n")
	rest = strings.TrimPrefix(rest, "---\r\n")
	idx := strings.Index(rest, "\n---")
	if idx == -1 {
		return "", fmt.Errorf("missing closing '---' delimiter for YAML frontmatter")
	}
	return rest[:idx], nil
}

func validate(fm *TaskFrontmatter) []string {
	var errs []string
	if strings.TrimSpace(fm.Name) == "" {
		errs = append(errs, "missing required field 'name'")
	} else if !kebabCase.MatchString(fm.Name) {
		errs = append(errs, fmt.Sprintf("'name' must be kebab-case (lowercase letters, digits, hyphens), got %q", fm.Name))
	}
	if strings.TrimSpace(fm.Description) == "" {
		errs = append(errs, "missing required field 'description'")
	}
	if strings.TrimSpace(fm.Change) == "" {
		errs = append(errs, "missing required field 'change'")
	}
	if strings.TrimSpace(fm.Status) == "" {
		errs = append(errs, "missing required field 'status'")
	} else if !validStatuses[fm.Status] {
		errs = append(errs, fmt.Sprintf("invalid 'status': %q (allowed: blocked, ready, needs-review, done)", fm.Status))
	}
	for i, rel := range fm.Relations {
		parts := strings.SplitN(rel, ":", 2)
		if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
			errs = append(errs, fmt.Sprintf("relations[%d]: %q must have form '<label>:<task-name>'", i, rel))
			continue
		}
		if !validRelationLabels[parts[0]] {
			errs = append(errs, fmt.Sprintf("relations[%d]: unknown label %q (allowed: blocked-by, discovered-from, enables)", i, parts[0]))
		}
		if !kebabCase.MatchString(parts[1]) {
			errs = append(errs, fmt.Sprintf("relations[%d]: task name %q must be kebab-case", i, parts[1]))
		}
	}
	return errs
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: validate-task-frontmatter <path>")
		os.Exit(64)
	}
	path := os.Args[1]
	content, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cannot read %s: %v\n", path, err)
		os.Exit(1)
	}
	fmBlock, err := extractFrontmatter(string(content))
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %v\n", path, err)
		os.Exit(2)
	}
	var parsed TaskFrontmatter
	if err := yaml.Unmarshal([]byte(fmBlock), &parsed); err != nil {
		fmt.Fprintf(os.Stderr, "%s: invalid YAML in frontmatter:\n  %v\n", path, err)
		os.Exit(2)
	}
	errs := validate(&parsed)
	if len(errs) > 0 {
		fmt.Fprintf(os.Stderr, "%s: task frontmatter validation failed:\n", path)
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "  - %s\n", e)
		}
		os.Exit(2)
	}
}

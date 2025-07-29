# wrangel.el

Extract and categorize org-mode todos from text using LLMs via gptel.el.

## Overview

This package provides functionality to extract actionable todos from any text and automatically categorize them into appropriate org files using Large Language Models through the gptel.el package.

## Features

- **Smart Extraction**: Uses LLM to identify actionable items from unstructured text
- **Automatic Categorization**: Sorts todos into `inbox.org`, `journal.org`, or `goals.org`
- **Multiple Input Methods**: Extract from buffer, region, or direct text input
- **Org-mode Integration**: Properly formatted org-mode TODO entries
- **Customizable**: Configurable system prompts and file mappings

## Installation

### Requirements

- Emacs 27.1 or later
- [gptel.el](https://github.com/karthink/gptel) 0.6.0 or later

### Manual Installation

1. Clone or download this repository
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/wrangel")
(require 'wrangel)
```

### Package Manager Installation

If using `use-package` with a local path:

```elisp
(use-package wrangel
  :load-path "/path/to/wrangel"
  :commands (wrangel-todo-from-buffer
             wrangel-todo-from-region
             wrangel-todo-from-text))
```

## Setup

1. Configure gptel.el with your preferred LLM backend
2. Ensure your org files directory is accessible
3. Optionally customize the system prompt and file mappings

## Usage

### Commands

- `M-x wrangel-todo-from-buffer` - Extract todos from entire buffer
- `M-x wrangel-todo-from-region` - Extract todos from selected region
- `M-x wrangel-todo-from-text` - Extract todos from input text

### Example Workflow

1. Open a file with notes, meeting minutes, or brainstorming text
2. Run `M-x wrangel-todo-from-buffer`
3. Watch as actionable todos are automatically extracted and filed

## Customization

### File Mappings

```elisp
(setq wrangel-files
      '(("inbox" . "inbox.org")
        ("journal" . "journal.org") 
        ("goals" . "goals.org")
        ("work" . "work-todos.org")))  ; Add custom categories
```

### System Prompt

Customize the LLM instructions by modifying `wrangel-system-prompt`.

## Example

Input text:
```
Meeting notes from today:
- Need to update documentation
- Should call the client about the proposal
- Want to learn more about machine learning
- Fix the bug in user login
```

Output todos:
- `inbox.org`: `* TODO Need to update documentation`
- `inbox.org`: `* TODO Should call the client about the proposal`  
- `goals.org`: `* TODO Want to learn more about machine learning`
- `inbox.org`: `* TODO Fix the bug in user login`

## License

GPL-3.0 or later

## Contributing

Issues and pull requests welcome!
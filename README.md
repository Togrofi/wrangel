# wrangel.el

Transform any text into actionable todos, zettelkasten notes, and summaries using LLMs via gptel.el for Emacs org-mode.

## Overview

This package provides comprehensive functionality to extract todos, atomic ideas, and summaries from any text using Large Language Models through the gptel.el package. It creates properly formatted org-mode entries, zettelkasten-style atomic notes, and integrates with org-node for enhanced knowledge management.

## Features

- **Smart TODO Extraction**: Uses LLM to identify actionable items from unstructured text
- **Automatic Categorization**: Sorts todos into `inbox.org`, `journal.org`, or `goals.org`
- **Atomic Ideas Extraction**: Creates zettelkasten-style atomic notes from text insights
- **Org-node Integration**: Generates atomic notes as separate org-node files with proper IDs
- **TLDR Summaries**: Creates concise summaries of text content
- **Digest Mode**: Combines todos, ideas, and TLDR into a single comprehensive entry
- **Multiple Input Methods**: Extract from buffer, region, or direct text input
- **Org-mode Integration**: Properly formatted org-mode entries and links
- **Customizable**: Configurable system prompts, file mappings, and directories

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
             wrangel-todo-from-text
             wrangel-ideas-from-buffer
             wrangel-ideas-from-region
             wrangel-ideas-from-text
             wrangel-tldr-from-buffer
             wrangel-tldr-from-region
             wrangel-tldr-from-text
             wrangel-digest-from-buffer
             wrangel-digest-from-region
             wrangel-digest-from-text))
```

## Setup

1. Configure gptel.el with your preferred LLM backend
2. Ensure your org files directory is accessible
3. Optionally customize the system prompt and file mappings

## Usage

### Commands

#### TODO Extraction
- `M-x wrangel-todo-from-buffer` - Extract todos from entire buffer
- `M-x wrangel-todo-from-region` - Extract todos from selected region
- `M-x wrangel-todo-from-text` - Extract todos from input text

#### Ideas Extraction
- `M-x wrangel-ideas-from-buffer` - Extract atomic ideas from entire buffer
- `M-x wrangel-ideas-from-region` - Extract atomic ideas from selected region
- `M-x wrangel-ideas-from-text` - Extract atomic ideas from input text

#### TLDR Summaries
- `M-x wrangel-tldr-from-buffer` - Create TLDR summary from entire buffer
- `M-x wrangel-tldr-from-region` - Create TLDR summary from selected region
- `M-x wrangel-tldr-from-text` - Create TLDR summary from input text

#### Digest Mode (All-in-One)
- `M-x wrangel-digest-from-buffer` - Extract todos, ideas, and TLDR from entire buffer
- `M-x wrangel-digest-from-region` - Extract todos, ideas, and TLDR from selected region
- `M-x wrangel-digest-from-text` - Extract todos, ideas, and TLDR from input text

### Example Workflows

#### Basic TODO Extraction
1. Open a file with notes, meeting minutes, or brainstorming text
2. Run `M-x wrangel-todo-from-buffer`
3. Watch as actionable todos are automatically extracted and filed

#### Comprehensive Digest Processing
1. Open a document with rich content (meeting notes, research, articles)
2. Run `M-x wrangel-digest-from-buffer`
3. Get todos filed in appropriate org files, atomic notes as separate org-node files, and a comprehensive digest entry linking everything together

#### Knowledge Management with Ideas
1. Read an interesting article or paper
2. Run `M-x wrangel-ideas-from-buffer`
3. Individual atomic notes are created as separate org-node files in your zettelkasten directory

## Customization

### Directory Configuration

```elisp
;; Main org files directory
(setq wrangel-todo-directory "~/org/")

;; Org-node atomic notes directory  
(setq wrangel-org-nodes-directory "~/org/zettel/")

;; Digest entries file
(setq wrangel-digest-file "~/org/wrangel-digest.org")
```

### File Mappings

```elisp
(setq wrangel-files
      '(("inbox" . "inbox.org")
        ("journal" . "journal.org") 
        ("goals" . "goals.org")
        ("work" . "work-todos.org")))  ; Add custom categories
```

### System Prompts

Customize the LLM instructions for different extraction types:

```elisp
;; Customize TODO extraction
(setq wrangel-system-prompt "Your custom todo extraction prompt...")

;; Customize atomic ideas extraction
(setq wrangel-ideas-system-prompt "Your custom ideas extraction prompt...")

;; Customize TLDR summaries
(setq wrangel-tldr-system-prompt "Your custom TLDR prompt...")
```

## Examples

### TODO Extraction Example

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

### Ideas Extraction Example

Input text with insights about functional programming creates individual org-node files like:

`~/org/zettel/12345-immutability-reduces-debugging.org`:
```org
* Immutability Reduces Debugging Complexity in Functional Programming
:PROPERTIES:
:ID: 12345-abcd-efgh-ijkl
:CREATED: 2024-01-15 14:30
:CATEGORY: programming
:FILETAGS: functional-programming immutability debugging
:END:

Functional programming's emphasis on immutability significantly reduces debugging complexity...
```

### Digest Mode Example

Running `wrangel-digest-from-buffer` creates:
1. Individual todos in appropriate org files
2. Atomic notes as separate org-node files  
3. A comprehensive digest entry in `wrangel-digest.org` with:
   - TLDR summary
   - Links to all generated atomic notes
   - Links to todo files
   - Original content for reference

## License

GPL-3.0 or later

## Contributing

Issues and pull requests welcome!
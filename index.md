## cl-liballegro
cl-liballegro provides bindings to Allegro5 which already provides fantastic documentation: https://liballeg.org/a5docs/trunk/

The documentation that will be provided here is therefore meant for idioms, usages, and general use of these bindings that are more specific to Common Lisp

## Basic usage
The most basic usage is 1-to-1 just uses the bindings "as is", with an example [here](https://github.com/resttime/cl-liballegro/blob/master/examples/simple-window.lisp)

In this case most function names simply match and there's not much different `al_init();` becomes `(al:init)`

## cffi
Occasionally there are times when dropping down to a lower level to using CFFI is required. This usually happens when a data structure is passed by reference to a function

```c
{
  ALLEGRO_EVENT event;
  bool running = true;
  while (running) process_event(&event);
}
```
In Common Lisp we use CFFI to allocate the structure, and then the corresponding functions from the Allegro5 library can take those in. Remember to free up the memory!
```lisp
(defparameter *running-p* t)
(let ((event (cffi:foreign-alloc '(:union al:event)))
  ;; do stuff with event here like an event loop
  (loop while *running-p* do (process-event event))
  ;; free after done
  (cffi:foreign-free event))
```

## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/resttime/cl-liballegro/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/resttime/cl-liballegro/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

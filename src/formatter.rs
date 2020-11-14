use super::sbnf::lexer::{Token};

use textwrap;

pub struct FormatterOptions {
    // Places global parameters at the top, followed by headers and then rules
    pub reorder: bool,
    pub ensure_trailing_newline: bool,
    pub max_line_length: u32,
}

impl FormatterOptions {
    pub fn default() -> FormatterOptions {
        FormatterOptions {
            reorder: true,
            ensure_trailing_newline: true,
            max_line_length: 80,
        }
    }
}

enum ElementType {
    Parameters,
    Header,
    Rule,
    Other,
}

struct Element<'a> {
    element_type: ElementType,
    tokens: &'a [Token<'a>],
}

pub fn format<'a>(options: FormatterOptions, tokens: &[Token<'a>]) -> String {
    let mut result = String::new();

    let mut elements = get_elements(tokens);

    if options.reorder {
        elements.sort_by_key(|elem| {
            match &elem.element_type {
                ElementType::Parameters => 0,
                ElementType::Header => 1,
                ElementType::Rule => 2,
                ElementType::Other => 3,
            }
        });
    }

    write_elements(&options, elements, &mut result);

    if options.ensure_trailing_newline && result.chars().last().map_or(true, |c| c != '\n') {
        result.push('\n');
    }

    result
}

fn get_elements<'a>(tokens: &'a [Token<'a>]) -> Vec<Element<'a>> {
    let mut element_start = 0;
    let mut elements = Vec::new();

    let mut index = 0;
    while index < tokens.len() {
        let token = &tokens[index];
        index += 1;

        match token {
            Token::Identifier(_) => {
                let mut element_type = ElementType::Rule;

                while index < tokens.len() {
                    let token = &tokens[index];
                    index += 1;

                    match token {
                        Token::HeaderString(_) => {
                            element_type = ElementType::Header;
                            break;
                        }
                        Token::SemiColon => {
                            break;
                        }
                        _ => {}
                    }
                }

                while index < tokens.len() && tokens[index].is_insignificant() {
                    index += 1;
                }

                elements.push(Element {
                    element_type,
                    tokens: &tokens[element_start..index]
                });
                element_start = index;
            }
            Token::OpenSquareBracket => {
                while index < tokens.len() {
                    let token = &tokens[index];
                    index += 1;

                    match token {
                        Token::CloseSquareBracket => {
                            break;
                        }
                        _ => {}
                    }
                }

                while index < tokens.len() && tokens[index].is_insignificant() {
                    index += 1;
                }

                elements.push(Element {
                    element_type: ElementType::Parameters,
                    tokens: &tokens[element_start..index],
                });
                element_start = index;
            }
            _ => {}
        }
    }

    assert!(index == tokens.len());
    if element_start != index {
        elements.push(Element { element_type: ElementType::Other, tokens: &tokens[element_start..index] });
    }

    elements
}

struct TokenIter<'a> {
    current: Option<Token<'a>>,
    iter: std::slice::Iter<'a, Token<'a>>,
}

impl<'a> TokenIter<'a> {
    fn next(&mut self) -> Option<Token<'a>> {
        self.current = self.iter.next().cloned();
        self.current
    }
}

fn write_elements<'a>(options: &FormatterOptions, elements: Vec<Element<'a>>, result: &mut String) {
    for (i, element) in elements.iter().enumerate() {
        let mut iter = TokenIter { current: None, iter: element.tokens.iter() };
        iter.next(); // prime

        if i != 0 {
            write_newlines(&mut iter, result);
        }

        match element.element_type {
            ElementType::Parameters => {

            },
            ElementType::Header => {
                write_element_header(options, &mut iter, result);
            },
            ElementType::Rule => {
                write_element_rule(options, &mut iter, result);
            },
            ElementType::Other => {
                let has_content = element.tokens.iter().find(|token| !token.is_whitespace_or_newline()).is_some();

                if !has_content {
                    return;
                }

                write_element_other(options, &mut iter, result);
            }
        }
    }
}

fn write_newlines<'a>(iter: &mut TokenIter<'a>, result: &mut String) {
    // Count the newlines
    let mut count = 0;
    while let Some(current) = iter.current {
        match current {
            Token::Newline => {
                count += 1;
            },
            Token::Whitespace(_) => {},
            _ => {
                break;
            }
        }

        iter.next();
    }

    let num_newlines =
        if count < 1 {
            1
        } else if count > 2 {
            2
        } else {
            count
        };

    for _ in 0..num_newlines {
        result.push('\n');
    }
}

fn write_element_header<'a>(options: &FormatterOptions, iter: &mut TokenIter<'a>, result: &mut String) {
    while let Some(current) = iter.current {
        match current {
            Token::Comment(_)
            | Token::Newline
            | Token::Whitespace(_) => {
                write_ignored(options, 0, iter, result);
            },
            Token::Identifier(_) => break,
            token => {
                result.push_str(&format!("{}", token));
                iter.next();
            },
        }
    }

    if let Some(identifier) = iter.current {
        result.push_str(&format!("{}", identifier));
        iter.next();
    }

    while let Some(current) = iter.current {
        match current {
            Token::Comment(_)
            | Token::Newline
            | Token::Whitespace(_) => {
                write_ignored(options, 0, iter, result);
            },
            Token::HeaderString(value) => {
                assert!(value.chars().nth(0).unwrap() == ':');
                let trimmed =
                    if value.chars().nth(1) == Some(' ') {
                        &value[2..]
                    } else {
                        &value[1..]
                    };

                result.push_str(": ");
                result.push_str(trimmed);
                iter.next();
                break;
            },
            token => {
                result.push_str(&format!("{}", token));
                iter.next();
            },
        }
    }

    write_element_other(options, iter, result);
}

fn write_element_rule<'a>(options: &FormatterOptions, iter: &mut TokenIter<'a>, result: &mut String) {

}

fn write_element_other<'a>(options: &FormatterOptions, iter: &mut TokenIter<'a>, result: &mut String) {
    while let Some(current) = iter.current {
        match current {
            Token::Comment(_)
            | Token::Newline
            | Token::Whitespace(_) => {
                write_ignored(options, 0, iter, result);
            },
            token => {
                result.push_str(&format!("{}", token));
                iter.next();
            }
        }
    }
}

fn write_ignored<'a>(options: &FormatterOptions, indentation: u32, iter: &mut TokenIter<'a>, result: &mut String) {
    while let Some(current) = iter.current {
        match current {
            Token::Comment(_) => {
                write_comment(options, indentation, iter, result);
            },
            Token::Newline
            | Token::Whitespace(_) => {
                iter.next();
            },
            _ => {
                break;
            },
        }
    }
}

#[derive(Debug)]
struct CommentParagraph<'a> {
    prefix: &'a str,
    indentation: u32,
    content: String,
    completed: bool,
}

impl<'a> CommentParagraph<'a> {
    fn is_compatible_prefix(&self, prefix: &'a str) -> bool {
        if self.completed {
            return false;
        }

        if prefix.chars().count() != self.indentation as usize {
            return false;
        }

        for chr in prefix.chars() {
            if chr != ' ' {
                return false;
            }
        }

        true
    }
}

fn get_comment_line(line: &str) -> (&str, &str, bool) {
    let mut end = line.len();
    let mut has_content = false;

    for (index, chr) in line.char_indices() {
        if chr.is_alphanumeric() {
            end = index;
            has_content = true;
            break;
        } else if !chr.is_ascii_whitespace() {
            has_content = true;
        }
    }

    (&line[..end], &line[end..], has_content)
}

fn write_comment<'a>(options: &FormatterOptions, indentation: u32, iter: &mut TokenIter<'a>, result: &mut String) {
    let mut paragraphs: Vec<CommentParagraph> = Vec::new();
    let mut newline_count = 0;

    while let Some(current) = iter.current {
        match current {
            Token::Comment(string) => {
                newline_count = 0;

                let (prefix, line, has_content) = get_comment_line(&string[1..]);

                // Empty lines complete the current paragraph
                if !has_content {
                    if !paragraphs.is_empty() {
                        paragraphs.last_mut().unwrap().completed = true;
                    }
                    iter.next();
                    continue;
                }

                // Start a new paragraph
                if paragraphs.is_empty() || !paragraphs[paragraphs.len() - 1].is_compatible_prefix(prefix) {
                    paragraphs.push(CommentParagraph {
                        prefix: prefix,
                        indentation: prefix.chars().count() as u32,
                        content: String::new(),
                        completed: false,
                    });
                }

                let paragraph = paragraphs.last_mut().unwrap();

                // Ignore the # at the front of the token
                if !paragraph.content.is_empty() {
                    paragraph.content.push(' ');
                }
                paragraph.content.push_str(line);
            },
            Token::Newline => {
                newline_count += 1;
                if newline_count == 2 {
                    iter.next();
                    break;
                }
            },
            Token::Whitespace(_) => {},
            _ => break,
        }

        iter.next();
    }

    let mut last_did_complete = false;
    for mut paragraph in paragraphs {
        if last_did_complete {
            write_indentation(indentation, result);
            result.push_str("#\n");
        }
        last_did_complete = paragraph.completed;

        if paragraph.prefix.chars().next() == Some(' ') {
            paragraph.prefix = &paragraph.prefix[1..];
            paragraph.indentation -= 1;
        }

        let wrap_width = options.max_line_length - (indentation + "# ".len() as u32 + paragraph.indentation);

        for (index, line) in textwrap::wrap_iter(&paragraph.content, wrap_width as usize).enumerate() {
            write_indentation(indentation, result);
            result.push_str("# ");

            if index == 0 {
                result.push_str(paragraph.prefix);
            } else {
                write_indentation(paragraph.indentation, result);
            }

            result.push_str(&*line);
            result.push('\n');
        }
    }

    if newline_count == 2 {
        result.push('\n');
    }
}

fn write_indentation(indentation: u32, result: &mut String) {
    for _ in  0..indentation {
        result.push(' ');
    }
}

#[cfg(test)]
mod tests {
    use crate::sbnf::lexer::lex;
    use super::*;

    fn fmt(source: &str) -> String {
        let grammar = lex(source);
        println!("{:?}", grammar.tokens);
        format(FormatterOptions::default(), &grammar.tokens)
    }

    #[test]
    fn format_comment() {
        assert!(fmt("") == "\n");
        assert!(fmt("\n") == "\n");
        assert!(fmt("\n\n") == "\n");
        assert!(fmt("\n\n\n") == "\n");
        assert!(fmt("#foo") == "# foo\n");
        assert!(fmt("# foo\n") == "# foo\n");
        assert!(fmt(" # foo\n") == "# foo\n");
        assert!(fmt(" # a long line that should get wrapped around due to the default max line length being 80 characters.") == "# a long line that should get wrapped around due to the default max line length\n# being 80 characters.\n");
        assert!(fmt("#foo\n#bar\n") == "# foo bar\n");
        assert!(fmt("#foo\n#\n#bar\n") == "# foo\n#\n# bar\n");
        assert!(fmt("#foo\n#\n#\n#bar\n") == "# foo\n#\n# bar\n");
        assert!(fmt("# foo\n\n# bar\n") == "# foo\n\n# bar\n");
        assert!(fmt("# Here's some examples:\n\
# * foo bar\n\
# * A longer example that should cause the line to wrap at \
80 characters onto the next line.\n\
#     Further indented paragraph here\n")
                == "# Here's some examples:\n\
# * foo bar\n\
# * A longer example that should cause the line to wrap at \
80 characters onto\n\
#   the next line.\n\
#     Further indented paragraph here\n");
        // Invalid
        assert!(fmt(";") == ";\n");
    }

    #[test]
    fn format_headers() {
        assert!(fmt("foo: bar") == "foo: bar\n");
        assert!(fmt("foo:bar") == "foo: bar\n");
        assert!(fmt("foo\n:bar") == "foo: bar\n");
        assert!(fmt("foo\n:bar") == "foo: bar\n");
        assert!(fmt("foo\n \t\n :bar") == "foo: bar\n");
        assert!(fmt("foo # foo\n:bar") == "foo# foo\n: bar\n");
        assert!(fmt("# foo\nfoo:bar") == "# foo\nfoo: bar\n");
        assert!(fmt("# foo\n\nfoo:bar") == "# foo\n\nfoo: bar\n");
    }
}

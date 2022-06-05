use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: generate_ast <output-directory>");
        std::process::exit(1);
    }
    let out_dir = Path::new(args[1].as_str());
    let types = Vec::from([
        // Binary will be the name of the class, the list on the right will be its fields
        "Binary   : Expr left, Token operator, Expr right",
        "Grouping : Expr expression",
        "Literal  : Object value",
        "Unary    : Token operator, Expr right",
    ]);

    if let Err(e) = write_ast(out_dir, "expr", types) {
        eprintln!("define_ast() returned error: {}", e.to_string());
        std::process::exit(1);
    }
}

struct AutogenStructFields {
    pub class: String,
    pub name: String,
}
impl AutogenStructFields {
    /// The input should be the text after the colon
    /// "Binary   : Expr left, Token operator, Expr right",
    fn new(struct_fields: &str) -> Vec<AutogenStructFields> {
        let mut out = Vec::new();
        let field_list = struct_fields.split(", ");
        for field in field_list {
            let split: Vec<String> = field
                .split(" ")
                .filter(|s| *s != "")
                .map(|s| s.to_string())
                .collect();

            let mut class = split[0].to_string();
            // I don't have Java's generic Object, just LiteralType
            if class == "Object" {
                class = "LiteralType".to_string();
            }
            out.push(AutogenStructFields {
                class,
                name: split[1].to_string(),
            });
        }
        out
    }
}
fn write_impl(
    file: &mut fs::File,
    struct_name: &str,
    struct_fields: &Vec<AutogenStructFields>,
) -> Result<(), io::Error> {
    writeln!(file, "impl {} {{", struct_name)?;
    // Create arglist for new()
    let mut args = String::new();
    for field in struct_fields {
        let arg = if field.class == "Expr" {
            format!("{}: Box<dyn Expr>, ", field.name)
        } else {
            format!("{}: {}, ", field.name, field.class)
        };
        args.push_str(&arg);
    }
    // remove the extra space
    args.pop();
    // remove the extra ,
    args.pop();
    writeln!(file, "    pub fn new({}) -> {} {{", args, struct_name)?;

    writeln!(file, "        {} {{", struct_name)?;
    for field in struct_fields {
        writeln!(file, "            {},", field.name)?;
    }
    writeln!(file, "        }}")?;
    writeln!(file, "    }}")?;
    writeln!(file, "}}")?;
    Ok(())
}

fn write_expr(file: &mut fs::File) -> Result<(), io::Error> {
    writeln!(file, "pub trait Expr {{}}")?;
    Ok(())
}

fn get_struct_name_and_fields(line: &str) -> (String, Vec<AutogenStructFields>) {
    let struct_name = line
        .split(":")
        .take(1)
        .map(|v| v.trim())
        .collect::<String>();
    let struct_fields = line.split(":").skip(1).collect::<String>();
    (struct_name, AutogenStructFields::new(&struct_fields))
}

fn write_ast(out_dir: &Path, base_name: &str, types: Vec<&str>) -> Result<(), io::Error> {
    let output_path: PathBuf = [out_dir, Path::new(&format!("{}.rs", base_name))]
        .iter()
        .collect();
    let mut file = fs::File::create(&output_path)?;
    writeln!(file, "use crate::token::LiteralType;")?;
    writeln!(file, "use crate::token::Token;")?;
    writeln!(file, "")?;
    write_expr(&mut file)?;
    writeln!(file, "")?;

    for type_def in types {
        let (struct_name, struct_fields) = get_struct_name_and_fields(type_def);
        writeln!(file, "pub struct {} {{", struct_name)?;
        write_struct_fields(&mut file, &struct_fields)?;
        writeln!(file, "}}")?;
        writeln!(file, "")?;

        writeln!(file, "impl Expr for {} {{}}", struct_name)?;

        write_impl(&mut file, &struct_name, &struct_fields)?;
    }

    println!("wrote output to: {}", output_path.to_string_lossy());
    Ok(())
}

fn write_struct_fields(
    file: &mut fs::File,
    fields: &Vec<AutogenStructFields>,
) -> Result<(), io::Error> {
    for field in fields {
        if field.class == "Expr" {
            writeln!(file, "    {}: Box<dyn Expr>,", field.name)?;
        } else {
            writeln!(file, "    {}: {},", field.name, field.class)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn parse_fields() {
        let fields = "Expr left, Token operator, Expr right";
        let vars = AutogenStructFields::new(fields);

        assert_eq!(vars.len(), 3);
        assert_eq!(vars[0].class, "Expr");
        assert_eq!(vars[0].name, "left");

        assert_eq!(vars[1].class, "Token");
        assert_eq!(vars[1].name, "operator");

        assert_eq!(vars[2].class, "Expr");
        assert_eq!(vars[2].name, "right");
    }

    #[test]
    fn parse_single_field() {
        let fields = "Object value";
        let vars = AutogenStructFields::new(fields);

        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].class, "LiteralType");
        assert_eq!(vars[0].name, "value");
    }

    #[test]
    fn struct_name_and_fields() {
        let line = "Binary   : Expr left, Token operator, Expr right";
        let (name, fields) = get_struct_name_and_fields(line);

        assert_eq!(name, "Binary");

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].class, "Expr");
        assert_eq!(fields[0].name, "left");

        assert_eq!(fields[1].class, "Token");
        assert_eq!(fields[1].name, "operator");

        assert_eq!(fields[2].class, "Expr");
        assert_eq!(fields[2].name, "right");
    }
}

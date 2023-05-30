/*
 * Rust GCode Parser
 * Copyright (c) 2023 Rust-GCode-Parser [https://github.com/MasterofNull/Rust-GCode-Parser]
 *
 * Copyright (c) 2023 Carlon LaMont
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */


///These are active to enable program to compile during standalone development 
#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(unused_variables)]

/*
==================================================================================================================================
          ///                        ///
         ///         Crates         ///
        ///                        ///
==================================================================================================================================
*/

/// Import Functions for Paralell Program Exection
use std::sync::{Arc, Mutex};
use rayon::prelude::*;
use std::thread;

/// Import Functions to Handle Trigonomic Functions
use regex::Regex;
use std::f64::consts::{PI, FRAC_PI_8};

// Import HashMap Function for storing macro variable data
use std::collections::HashMap;

// Import nom for data parsing, Removed 'sequence::{separate_list0, separate_list1}' error::{Error, ErrorKind},
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{map_res, opt},
    multi::{many0, separated_list1},
    error::{Error, ErrorKind},
    sequence::{delimited, preceded},
    IResult,
};


/*
==================================================================================================================================
          ///                            ///
         ///         Structures         ///
        ///                            ///
==================================================================================================================================
*/

#[derive(Debug)]
struct GCodeProgram {
    commands: Vec<GCodeCommand>,
}

// Define a struct to hold the parsed G-code commands
#[derive(Debug)]
struct CommandLine { 
    line_number: String,
    command: String,
    arguments: HashMap<String, String>,
}
 
#[derive(Debug)]
struct CommandString {
    label: Option<String>,
    code: String,
    arguments: Vec<Argument>,
}

#[derive(Debug)]
struct MCommand {
    code: String,
    parameters: Vec<(char, f64)>,
}

#[derive(Debug)]
struct GCommand {
    codes: Vec<char>,
}

#[derive(Debug)]
struct IfStatement {
    condition: Expression,
    commands: Vec<GCodeCommand>,
    else_if_conditions: Vec<(Expression, Vec<GCodeCommand>)>,
    else_commands: Option<Vec<GCodeCommand>>,
}

#[derive(Debug)]
struct WhileLoop {
    condition: Expression,
    commands: Vec<GCodeCommand>,
}

#[derive(Debug)]
struct VariableCall {
    variable: char,
}

#[derive(Debug)]
struct MacroCall {
    name: String,
    parameters: Vec<(char, f64)>,
}

#[derive(Debug)]
struct VariableAssignment {
    variable: char,
    value: f64,
}

#[derive(Debug)]
struct BinaryOperation {
    left: Box<Expression>,
    operator: BinaryOperator,
    right: Box<Expression>,
}


/*
==================================================================================================================================
          ///                             ///
         ///         Enumerators         ///
        ///                             ///
==================================================================================================================================
*/

#[derive(Debug)]
enum Expression {
    Constant(f64),
    Number(f64),
    Variable(char),
    FunctionCall(String, Vec<Expression>),
    BinaryOperation(Box<BinaryOperation>),
}

#[derive(Debug)]
enum Command {
    Comment(String),
    GCode(String),
    If(Expression, String),
    ElseIf(Expression, String),
    Else(String),
    While(Expression, String),
    SetVariable(String, Expression),
    FunctionCall(Expression),
}
 
#[derive(Debug)]
enum GCodeCommand {
    NCommand(String),
    MCommand(MCommand),
    GCommand(GCommand),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    MacroCall(MacroCall),
    VariableAssignment(VariableAssignment),
}

#[derive(Debug)]
enum Argument {
    Float(f64),
    Label(String),
    BracketedCommand(String),
    BracketedCommands(Vec<String>),
    MathFunction(String),
    ControlFunction(String),
    Expression(String),
    Assignment(String),
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/*  
=================================================================================================================================
          ///                                      ///
         ///         Concurrent Execution         ///
        ///                                      ///
=================================================================================================================================
*/

// Define the function type for parsed commands
type ParsedCommandFn = dyn Fn(&str) -> Option<GCodeCommand> + Send + Sync + 'static;

// Function to process a G-code line
fn process_gcode_line(
    line: String, 
    output: Arc<Mutex<Vec<String>>>, 
    commands: &[GCodeCommand], 
    gcode: &[String], 
    parsed_commands: Arc<Mutex<ParsedCommandFn>>,
) {
    // Pass the `commands` vector as an argument to the `process_gcode_line` function
    process_gcode_line(line, output, &commands, &gcode, parsed_commands);

    // Create a vector to hold the processed lines
    let mut output = Vec::new();
    //let mut output = output.lock().unwrap();
    
    // Create a regular expression object
    let trig_regex = Regex::new(TRIG_PATTERN).unwrap();
    //let trig_regex = Regex::new(r"(?i)[A-Z_]+(\s)*(\(.*?\))?").unwrap();
    
    // Search for trigonometric functions in the line
    let matches: Vec<_> = trig_regex.find_iter(&line).collect();

    if matches.is_empty() {
        output.push(line);
        return;
    }

    for mat in matches {
        let parsed_commands_clone = Arc::clone(&parsed_commands);
        let command_text = mat.as_str();
        let command = parsed_commands_clone.lock().unwrap()(command_text);
        let function = &line[mat.start()..mat.start() + 1];
        let expression = mat.as_str();
        
        // Evaluate the trigonometric expression
        if let Some(value) = evaluate_trig(expression) {
            // Replace the expression in the G-code line with the evaluated value
            let processed_line = line.replace(expression, &value.to_string());

            // Push the processed line to the output vector
            output.push(processed_line);
        }

        match command {
            Some(GCodeCommand::NCommand(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::MCommand(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::GCommand(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::IfStatement(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::WhileLoop(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::MacroCall(_)) => {
                output.push(line);
                return;
            }
            Some(GCodeCommand::VariableAssignment(_)) => {
                output.push(line);
                return;
            }
            None => {
                output.push(line);
                return;
            }
        }

    }

    let parsed_commands_clone = Arc::clone(&parsed_commands);

    // Define a vector to hold the parsed commands
    let commands: Vec<GCodeCommand> = Vec::new();

    // Create a mutable reference to the function
    let parsed_commands  = Arc::clone(&parsed_commands);
    
    // Create a vector to hold the child threads
    let mut threads = vec![];
    
    // Iterate over the G-code lines and spawn a thread for each line
    for line in gcode {
        // Clone the shared state for each thread
        let parsed_commands_clone = Arc::clone(&parsed_commands);

        // Spawn a new thread to parse and process the command
        let thread = thread::spawn(move || {
            // Parse the command
            let command = parsed_commands_clone.lock().unwrap()(line);

            // Lock the parsed_commands mutex and add the command to the shared state
            let mut parsed_commands = parsed_commands_clone.lock().unwrap();
            if let Some(command) = command {
                parsed_commands.push(command);
            }
        });

        // Store the thread in the vector
        threads.push(thread);
        }

    // Wait for all threads to complete
    for thread in threads {
        thread.join().unwrap();
    }

    // Retrieve the parsed commands from the shared state
    let parsed_commands = parsed_commands.lock().unwrap();

    // Process the parsed commands concurrently (Changed process_command to parse_command)
    parsed_commands_clone.lock().unwrap().par_iter().for_each(|command| {
    //parsed_commands.par_iter().for_each(|command| {
        process_command(command);
    });

    // Push the original line to the output vector
    output.push(line);
}


/*  
=================================================================================================================================
          ///                                    ///
         ///         G-Codes and Macros         ///
        ///                                    ///
=================================================================================================================================
*/

// Function to parse a single G-code command
fn parsed_commands(line: &str) -> Option<GCodeCommand> {
    //let parsed_commands = parsed_commands_clone.lock().unwrap();
    let result = command_line_parser(line);
    if let Ok((_, command_line)) = result {
        Some(parse_gcode_command(command_line))
    } else {
        None
    }
}

fn process_command(command: &GCodeCommand) {
    match command {
        GCodeCommand::NCommand(number) => {
            // Process N command
            println!("N command: {}", number);
        }
        GCodeCommand::MCommand(m_command) => {
            // Process M command
            println!("M command: {:?}", m_command);
        }
        GCodeCommand::GCommand(g_command) => {
            // Process G command
            println!("G command: {:?}", g_command);
        }
        GCodeCommand::IfStatement(if_statement) => {
            // Process if statement
            println!("If statement: {:?}", if_statement);
        }
        GCodeCommand::WhileLoop(while_loop) => {
            // Process while loop
            println!("While loop: {:?}", while_loop);
        }
        GCodeCommand::MacroCall(macro_call) => {
            // Process macro call
            println!("Macro call: {:?}", macro_call);
        }
        GCodeCommand::VariableAssignment(variable_assignment) => {
            // Process variable assignment
            println!("Variable assignment: {:?}", variable_assignment);
        }
    }
}

fn parse_gcode_command(command_line: CommandLine) -> GCodeCommand {
    match command_line.command.as_str() {
        "N" => GCodeCommand::NCommand(command_line.line_number),
        "M" => GCodeCommand::MCommand(parse_m_command(command_line.arguments)),
        "G" => GCodeCommand::GCommand(parse_g_command(command_line.arguments)),
        "IF" => GCodeCommand::IfStatement(parse_if_statement(command_line.arguments)),
        "WHILE" => GCodeCommand::WhileLoop(parse_while_loop(command_line.arguments)),
        _ => GCodeCommand::MacroCall(parse_macro_call(
            command_line.command,
            command_line.arguments,
        )),
    }
}

fn parse_m_command(arguments: HashMap<String, String>) -> MCommand {
    let code = arguments.get("M").unwrap().to_string();

    let parameters: Vec<(char, f64)> = arguments
        .into_iter()
        .filter(|(key, _)| key.starts_with(char::is_alphabetic))
        .filter_map(|(key, value)| {
            let parameter = key.chars().next().unwrap();
            let value = value.parse::<f64>().ok()?;
            Some((parameter, value))
        })
        .collect();

    MCommand { code, parameters }
}

fn parse_macro_call(name: String, arguments: HashMap<String, String>) -> MacroCall {
    let parameters: Vec<(char, f64)> = arguments
        .into_iter()
        .filter(|(key, _)| key.starts_with(char::is_alphabetic))
        .filter_map(|(key, value)| {
            let parameter = key.chars().next().unwrap();
            let value = value.parse::<f64>().ok()?;
            Some((parameter, value))
        })
        .collect();

    MacroCall { name, parameters }
}

fn parse_g_command(arguments: HashMap<String, String>) -> GCommand {
    let codes = arguments
        .get("G")
        .unwrap_or(&"".to_string())
        .chars()
        .collect();
 
    GCommand { codes }
}

fn parse_command_letter(input: &str) -> IResult<&str, String> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char('G')(input)?;
    let (input, _) = multispace1(input)?;

    Ok((input, "G".to_string()))
}


/*  
=================================================================================================================================
          ///                                 ///
         ///         Logic and Loops         ///
        ///                                 ///
=================================================================================================================================
*/

fn parse_if_statement(arguments: HashMap<String, String>) -> IfStatement {
    let condition_str = arguments.get("CONDITION").unwrap();
    
    let condition = parse_expression(condition_str);

    let mut commands = Vec::new();
    let mut else_if_conditions = Vec::new();
    let mut else_commands = None;

    for (key, value) in arguments {
        match key.as_str() {
            "THEN" => commands = parse_gcode_block(&value),
            "ELSE_IF" => {
                let else_if_condition = parse_expression(&value);
                let else_if_commands = parse_gcode_block(&value);
                else_if_conditions.push((else_if_condition, else_if_commands));
            }
            "ELSE" => else_commands = Some(parse_gcode_block(&value)),
            _ => (),
        }
    }

    IfStatement {
        condition,
        commands,
        else_if_conditions,
        else_commands,
    }
}

fn parse_while_loop(arguments: HashMap<String, String>) -> WhileLoop {
    let condition_str = arguments.get("CONDITION").unwrap();

    let condition = parse_expression(condition_str);

    let commands = parse_gcode_block(arguments.get("DO").unwrap());

    WhileLoop { condition, commands }
}


/*  
=================================================================================================================================
          ///                              ///
         ///         Expressions          ///
        ///                              ///
=================================================================================================================================
*/

fn parse_expression(expression: &str) -> Expression {
    match expression_parser(expression) {
        Ok((_, parsed_expression)) => parsed_expression,
        Err(_) => Expression::Constant(0.0), // Default to 0.0 if parsing fails
    }
}

fn parse_gcode_block(block: &str) -> Vec<GCodeCommand> {
    let commands: Vec<GCodeCommand> = block
        .split('\n')
        .filter_map(|line| parsed_commands(line))
        .collect();

    commands
}

fn expression_parser(input: &str) -> IResult<&str, Expression> {
    preceded(
        multispace0,
        alt((
            constant_expression_parser,
            program_variable_expression_parser,
            variable_expression_parser,
            function_call_expression_parser,
            binary_operation_parser,
        )),
    )(input)
}

fn constant_expression_parser(input: &str) -> IResult<&str, Expression> {
    map_res(digit1, |digits: &str| {
        digits.parse::<f64>()
            .map(Expression::Constant)
            .map_err(|_| Error::from_error_kind(digits, ErrorKind::Digit))
    })(input)
}

fn program_variable_expression_parser(input: &str) -> IResult<&str, Expression> {
    map_res(
        delimited(char('#'), digit1, char('.').or(char('E')).or(char('e'))),
        |digits: &str| {
            digits.parse::<f64>()
                .map(Expression::Number)
                .map_err(|_| Error::from_error_kind(digits, ErrorKind::Digit))
        },
    )(input)
}

fn variable_expression_parser(input: &str) -> IResult<&str, Expression> {
    map_res(alphanumeric1, |var: &str| {
        var.chars()
            .next()
            .map(Expression::Variable)
            .ok_or(Error::from_error_kind(var, ErrorKind::AlphaNumeric))
    })(input)
}

fn function_call_expression_parser(input: &str) -> IResult<&str, Expression> {
    map_res(
        separated_list1(
            multispace1,
            alt((
                map_res(alphanumeric1, |s: &str| {
                    s.parse::<f64>().map(Expression::Constant)
                }),
                map_res(alphanumeric1, |s: &str| {
                    Ok(Expression::FunctionCall(s.to_string(), vec![]))
                }),
            )),
        ),
        |expressions: Vec<Expression>| {
            let mut iter = expressions.into_iter();
            let function_name = match iter.next() {
                Some(Expression::FunctionCall(name, _)) => name,
                Some(Expression::Constant(constant)) => {
                    return Ok(Expression::Constant(constant));
                }
                Some(_) => return Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
                None => return Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
            };
            let arguments = iter.collect();
            Ok(Expression::FunctionCall(function_name, arguments))
        },
    )(input)
}


/*  
=================================================================================================================================
          ///                           ///
         ///         Operators         ///
        ///                           ///
=================================================================================================================================
*/

fn binary_operation_parser(input: &str) -> IResult<&str, Expression> {
    let (input, left) = expression_parser(input)?;

    let (input, operator) = alt((
        map_res(tag("+"), |_| Ok(BinaryOperator::Add)),
        map_res(tag("-"), |_| Ok(BinaryOperator::Subtract)),
        map_res(tag("*"), |_| Ok(BinaryOperator::Multiply)),
        map_res(tag("/"), |_| Ok(BinaryOperator::Divide)),
    ))(input)?;

    let (input, right) = expression_parser(input)?;

    let binary_operation = BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    };

    Ok((input, Expression::BinaryOperation(Box::new(binary_operation))))
}

fn command_line_parser(input: &str) -> IResult<&str, CommandLine> {
    let (input, line_number) = parse_line_number(input)?;
    let (input, command) = parse_command_letter(input)?;
    let (input, arguments) = parse_arguments(input)?;

    let command_line = CommandLine {
        line_number,
        command,
        arguments,
    };

    Ok((input, command_line))
}

fn parse_line_number(input: &str) -> IResult<&str, String> {
    let (input, _) = multispace0(input)?;
    let (input, line_number) = map_res(digit1, |digits: &str| {
        Ok(digits.to_string())
    })(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, line_number))
}


/*  
=================================================================================================================================
          ///                           ///
         ///         Arguments         ///
        ///                           ///
=================================================================================================================================
*/

fn parse_arguments(input: &str) -> IResult<&str, HashMap<String, String>> {
    let (input, _) = multispace0(input)?;
    let (input, argument_list) = many0(parse_argument)(input)?;
    let (input, _) = multispace0(input)?;

    let arguments: HashMap<String, String> = argument_list.into_iter().collect();

    Ok((input, arguments))
}

fn parse_argument(input: &str) -> IResult<&str, (String, String)> {
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_argument_name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, value) = parse_argument_value(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, (name, value)))
}

fn parse_argument_name(input: &str) -> IResult<&str, String> {
    let (input, _) = multispace0(input)?;
    let (input, name) = alphanumeric1(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, name.to_string()))
}

fn parse_argument_value(input: &str) -> IResult<&str, String> {
    let (input, _) = multispace0(input)?;
    let (input, value) = alt((
        delimited(char('"'), take_until("\""), char('"')),
        map_res(alphanumeric1, |s: &str| Ok(s.to_string())),
    ))(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, value.to_string()))
}


/*
==================================================================================================================================
          ///                                        ///
         ///         Trigonomic Expressions         ///
        ///                                        ///
==================================================================================================================================
*/

// Regular expression pattern to match trigonometric functions in G-code
const TRIG_PATTERN: &str = r"([A-Z])(.*?)(\[[A-Za-z]+\(.+?\).*?\])";

// Function to evaluate trigonometric expressions
fn evaluate_trig(expression: &str) -> Option<f64> {
    // Remove the square brackets and extract the function and argument
    let expression = expression.trim_matches(|c| c == '[' || c == ']');
    let parts: Vec<&str> = expression.split('(').collect();
    let function = parts.get(0)?;
    let argument = parts.get(1)?;

    // Extract the value within the parentheses and evaluate the trigonometric function
    let value = argument[..argument.len() - 1].parse::<f64>().ok()?;
    match function {
        &"SIN" => Some((value * FRAC_PI_8).sin()),
        &"COS" => Some((value * FRAC_PI_8).cos()),
        &"TAN" => Some((value * FRAC_PI_8).tan()),
        _ => None,
    }
}


/*
==================================================================================================================================
          ///                      ///
         ///         Main         ///
        ///                      ///
==================================================================================================================================
*/

fn main() {
    // Example G-code
    let gcode = Arc::new(Mutex::new(String::from(
        "
        N000 O0001 (This is a comment on code)
        N001 M06 T1
        N002 G0 G20 G54 X2.00 Y2.00 Z4.00
        N003 G01 G43 F20.0 Z-5.00 H01 M08
        N004 M106 S255
        N005 IF #1 > 0 GOTO N012
        N006 ELSE IF #2 == 0 THEN [G1 X50 Y60 Z70]
        N007 ELSE [G1 X70 Y80 Z90]
        N008 [G1 X10 Y20 Z30 #3 = #3 + 1]
        N009 M98 P0100
        N010 #1000=100 #2000=X20.45 #3000=Y456.908 #4000=Z345.875
        N011 G0 X#1000 Y#2000 Z#3000
        N012 IF [#100 == 0] THEN #100 = 1 (Avoid dividing by zero!)
        N013 IF [#100 <= 0] THEN #100 = 10
        N014 ELSE [#100 >= 0] THEN #100 = -10
        N015 WHILE [#3 == 10] DO1
        N016 IF [#100 EQ 0] THEN #100 = 1 (Avoid dividing by zero!)
        N017 IF [#100 GT 0] THEN #100 = 10
        N018 IF [#100 LT 0] THEN #100 = -10
        N019 ELSE #100=0
        N020 END1
        N021 #1=SIN[#2]
        N022 #1=COS[#3]
        N023 #1=TAN[#4]
        N024 #1=ACOS[#5]
        N025 #1=ATAN[#6]/[#7]
        N026 #1=SQRT=[#8]
        N027 #1=ABS[#9]
        N028 #1=LN[#10]
        N029 #1=EXP[#11] (Exponent base e)
        N030 #1=ADP[#12] (Add a decimal point to the end of the number)
        N031 #1=ROUND[#13]
        N032 #1=FUP[#14]
        N033 #1=FIX[#15]
        N034 #1=BIN[#16] (Convert from BCD to binary)
        N035 #1=BCD[#17] (Convert from binary to BCD)
        N036 DPRNT [OUTPUT*TIME:**#3000[60]]
        N037 M30
        ",
    )));

    let parsed_commands: Arc<Mutex<ParsedCommandFn>> = Arc:new(Mutex::new(Box::new(parsed_commands)));

    // Vector to store the processed G-code lines
    let output = Arc::new(Mutex::new(Vec::new()));

    // Split the G-code into individual lines
    let lines: Vec<String> = gcode
        .lock()
        .unwrap()
        .trim()
        .split('\n')
        .map(|line| String::from(line.trim()))
        .collect();

    // Process the G-code lines in parallel using multiple threads
    let handles: Vec<_> = lines
        .into_iter()
        .map(|line| {
            let gcode = Arc::clone(&gcode);
            let output = Arc::clone(&output);
            thread::spawn(move || process_gcode_line(line:, output:, commands:, gcode:, parsed_commands:,))
        })
        .collect();

    // Wait for all threads to finish
    for handle in handles {
        handle.join().unwrap();
    }

    // Print the processed G-code lines
    let output = output.lock().unwrap();
    for line in &*output {
        println!("{}", line);
    }
}


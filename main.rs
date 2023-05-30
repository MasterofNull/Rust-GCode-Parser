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

use std::sync::{Arc, Mutex};

use std::thread;

// Import HashMap Function for storing macro variable data
use std::collections::HashMap;

use rayon::prelude::*;

// Import nom for data parsing
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1, multispace0, multispace1},
    character::complete::{alphanumeric1, char, digit1, multispace0},
    combinator::{recognize, map, map_res, opt},
    error::{Error, ErrorKind},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_list0, separated_list1, terminated, tuple},
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
struct GCodeCommand {
    line_number: String,
    command: String,
    arguments: HashMap<String, String>,
}

#[derive(Debug)]
struct Command {
    label: String,
    code: String,
    arguments: Vec<Expression, Argument>,
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
    variable: usize,
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
enum Error<'a> {
    ParseIntError(std::num::ParseFloatError),
    UnknownFunction(&'a str),
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
          ///                                    ///
         ///         G-Codes and Macros         ///
        ///                                    ///
=================================================================================================================================
*/

// Function to parse and process G-code commands concurrently
fn parse_and_process_gcode_concurrently(gcode: &[String]) {
    // Create a shared mutable state to store the parsed commands
    let parsed_commands: Arc<Mutex<Vec<GCodeCommand>>> = Arc::new(Mutex::new(Vec::new()));

    // Create a vector to hold the child threads
    let mut threads = vec![];

    // Iterate over the G-code lines and spawn a thread for each line
    for line in gcode {
        // Clone the shared state for each thread
        let parsed_commands_clone = Arc::clone(&parsed_commands);

        // Spawn a new thread to parse and process the command
        let thread = thread::spawn(move || {
            // Parse the command
            let command = parse_command(line);

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

    // Process the parsed commands concurrently
    parsed_commands.par_iter().for_each(|command| {
        process_command(command);
    });
}

// Function to parse a single G-code command
fn parse_command(line: &str) -> Option<GCodeCommand> {
    let tokens: Vec<&str> = line.split_whitespace().collect();

    if tokens.is_empty() {
        return None;
    }

    let line_number = tokens[0].to_string();
    let command = tokens[1..].join(" ");

    let mut arguments = HashMap::new();
    for token in tokens[2..].iter() {
        let parts: Vec<&str> = token.split('=').collect();
        if parts.len() == 2 {
            let key = parts[0].trim_start_matches('#').to_string();
            let value = parts[1].to_string();
            arguments.insert(key, value);
        }
    }

    Some(GCodeCommand {
        line_number,
        command,
        arguments,
    })
}

// Function to parse a G-code command
fn parse_g_code_command(input: &str) -> IResult<&str, GCodeCommand> {
    alt((
        parse_n_command,
        parse_m_command,
        parse_g_command,
        parse_if_statement,
        parse_while_loop,
        parse_macro_call,
        parse_variable_assignment,
        parse_comment,
    ))(input)
}

fn parse_g_code_program(input: &str) -> IResult<&str, GCodeProgram> {
    let (input, _) = multispace0(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, GCodeProgram { commands }))
}

fn parse_g_command(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('G')(input)?;
    let (input, codes) = many1(char_digit)(input)?;
    Ok((input, GCodeCommand::GCommand(GCommand { codes })))
}

fn parse_m_command(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('M')(input)?;
    let (input, code) = digit1(input)?;
    let (input, parameters) = many0(parse_parameter)(input)?;
    Ok((
        input,
        GCodeCommand::MCommand(MCommand {
            code: format!("M{}", code),
            parameters,
        }),
    ))
}

fn parse_macro_call(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, name) = alphanumeric1(input)?;
    let (input, _) = multispace0(input)?;
    let (input, parameters) = many0(parse_parameter)(input)?;
    Ok((
        input,
        GCodeCommand::MacroCall(MacroCall { name: name.to_string(), parameters }),
    ))
}

fn parse_n_command(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('N')(input)?;
    let (input, code) = digit1(input)?;
    let (input, _) = multispace0(input)?;
    let (input, comment) = opt(parse_comment)(input)?;
    Ok((
        input,
        GCodeCommand::NCommand(format!("N{}{}", code, comment.unwrap_or(""))),
    ))
}

fn parse_label(input: &str) -> IResult<&str, &str> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("N")(input)?;
    let (input, label) = take_until(" ")(input)?;
    Ok((input, label))
}

fn parse_code_number(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('N')(input)?;
    let (input, code) = take_until(" ")(input)?;
    Ok((input, code))
}

fn parse_gcode(input: &str) -> Result<Vec<Command>, nom::Err<Error<&str>>> {
    let (_, commands) = separated_list1(char('\n'), parse_code)(input)?;
    Ok(commands)
}


/*  
=================================================================================================================================
          ///                                                     ///
         ///         Variables, Comments, and Parameters         ///
        ///                                                     ///
=================================================================================================================================
*/

fn parse_variable(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('#')(input)?;
    let (input, variable) = alphanumeric1(input)?;
    Ok((input, variable))
}
fn parse_variable(input: &str) -> IResult<&str, Expression, Error> {
    map(preceded(char('#'), digit1), |var_str: &str| {
        Expression::Variable(var_str.into())
    })(input)
}

fn parse_variable(input: &str) -> IResult<&str, Expression> {
    let (input, variable) = char_alphabetic(input)?;
    Ok((input, Expression::Variable(variable.to_string())))
}

fn parse_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = char(' ')(input)?;
    let (input, comment) = take_until("\n")(input)?;
    Ok((input, comment))
}

fn parse_parameter(input: &str) -> IResult<&str, (char, f64)> {
    let (input, _) = char(' ')(input)?;
    let (input, param) = alphanumeric1(input)?;
    let (input, _) = char('=')(input)?;
    let (input, value) = parse_expression(input)?;
    Ok((input, (param.chars().next().unwrap(), value)))
}

fn parse_macro_parameters(input: &str) -> IResult<&str, Vec<(char, f64)>> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, parameters) = separated_list1(
        preceded(multispace0, char(',')),
        preceded(multispace0, parse_parameter),
    )(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, parameters))
}
    
fn parse_variable_assignment(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, variable) = char_digit(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, value) = parse_float(input)?;
    Ok((
        input,
        GCodeCommand::VariableAssignment(VariableAssignment { variable, value }),
    ))
}

fn parse_parameter(input: &str) -> IResult<&str, (char, f64)> {
    let (input, code) = char_alphabetic(input)?;
    let (input, _) = char('=')(input)?;
    let (input, value) = parse_float(input)?;
    Ok((input, (code, value)))
}


/*
==================================================================================================================================
          ///                                           ///
         ///         Operators and Expressions         ///
        ///                                           ///
==================================================================================================================================
*/

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_number,
        parse_variable,
        parse_function_call,
        parse_parenthesized_expression,
    ))(input)
}

fn parse_number(input: &str) -> IResult<&str, Expression> {
    let (input, number) = parse_float(input)?;
    Ok((input, Expression::Number(number)))
}

fn parse_function_call(input: &str) -> IResult<&str, Expression> {
    let (input, function) = char_alphabetic(input)?;
    let (input, _) = char('(')(input)?;
    let (input, arguments) = separated_list1(char(','), parse_expression)(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, Expression::FunctionCall(function.to_string(), arguments)))
}
fn parse_function_call(input: &str) -> IResult<&str, Expression, Error> {
    let parse_arguments = delimited(
        char('['),
        separated_list1(char(','), parse_expression),
        char(']'),
    );

    let parse_function = map(
    tuple((
        alt((char('['), char('('))),
        multispace0,
        digit1,
        multispace0,
        opt(parse_arguments),
        multispace0,
        alt((char(']'), char(')'))),
    )),
    |(_, _, func_name, _, args, _, _)| {
        let function_name = match func_name {
            "SIN" | "COS" | "TAN" | "ACOS" | "ATAN" | "SQRT" | "ABS" | "LN" | "EXP" | "ADP"
            | "ROUND" | "FUP" | "FIX" | "BIN" | "BCD" => func_name.into(),
            _ => return Err(Error::UnknownFunction(func_name)),
        };

        let arguments = args.unwrap_or_default();

        Expression::FunctionCall(function_name, arguments)
    },
);

parse_function(input)
}

fn parse_parenthesized_expression(input: &str) -> IResult<&str, Expression> {
    delimited(char('('), parse_expression, char(')'))(input)
}

fn parse_variable_expression(input: &str) -> IResult<&str, Expression> {
    let (input, variable) = parse_variable(input)?;
    Ok((input, Expression::Variable(variable.chars().next().unwrap())))
}

fn parse_binary_operation_expression(input: &str) -> IResult<&str, Expression> {
    let (input, left) = alt((parse_variable_expression, parse_constant_expression))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, operator) = parse_binary_operator(input)?;
    let (input, _) = multispace0(input)?;
    let (input, right) = parse_expression(input)?;
    Ok((
        input,
        Expression::BinaryOperation(Box::new(BinaryOperation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })),
    ))
}


/*
==================================================================================================================================
          ///                           ///
         ///         Arguments         ///
        ///                           ///
==================================================================================================================================
*/

fn parse_argument(input: &str) -> IResult<&str, Argument> {
    alt((
        parse_float,
        parse_label_argument,
        parse_bracketed_command,
        parse_bracketed_commands,
        parse_math_function,
        parse_control_function,
        parse_expression,
        parse_assignment,
    ))(input)
}

fn parse_float(input: &str) -> IResult<&str, f64> {
    let (input, sign) = opt(alt((char('+'), char('-'))))(input)?;
    let (input, whole) = digit1(input)?;
    let (input, fraction) = opt(preceded(char('.'), digit1))(input)?;
    let number = format!("{}{}", sign.unwrap_or(""), whole);
    if let Some(fraction) = fraction {
        let number = format!("{}{}", number, fraction);
        let number = number.parse::<f64>().unwrap();
        Ok((input, number))
        } else {
            let number = number.parse::<f64>().unwrap();
        Ok((input, number))
    }
}

fn parse_label_argument(input: &str) -> IResult<&str, Argument> {
    let (input, label) = parse_label(input)?;
    Ok((input, Argument::Label(label.to_string())))
}

fn parse_bracketed_command(input: &str) -> IResult<&str, Argument> {
    let (input, command) = parse_bracketed_text(input)?;
    Ok((input, Argument::BracketedCommand(command)))
}

fn parse_bracketed_commands(input: &str) -> IResult<&str, Argument> {
    let (input, commands) = parse_bracketed_text(input)?;
    let commands = commands
        .split_whitespace()
        .map(|command| command.to_string())
        .collect();
    Ok((input, Argument::BracketedCommands(commands)))
}

fn parse_bracketed_text(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('[')(input)?;
    let (input, text) = take_until("]")(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, text))
}

fn parse_math_function(input: &str) -> IResult<&str, Argument> {
    let (input, function) = parse_bracketed_text(input)?;
    Ok((input, Argument::MathFunction(function)))
}

fn parse_control_function(input: &str) -> IResult<&str, Argument> {
    let (input, function) = parse_bracketed_text(input)?;
    Ok((input, Argument::ControlFunction(function)))
}

fn parse_expression(input: &str) -> IResult<&str, Argument> {
    let (input, expression) = parse_bracketed_text(input)?;
    Ok((input, Argument::Expression(expression)))
}

fn parse_assignment(input: &str) -> IResult<&str, Argument> {
    let (input, assignment) = parse_bracketed_text(input)?;
    Ok((input, Argument::Assignment(assignment)))
}

fn parse_binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    let (input, operator) = alt((
        map(char('+'), |_| BinaryOperator::Add),
        map(char('-'), |_| BinaryOperator::Subtract),
        map(char('*'), |_| BinaryOperator::Multiply),
        map(char('/'), |_| BinaryOperator::Divide),
    ))(input)?;
    Ok((input, operator))
}

fn char_digit(input: &str) -> IResult<&str, char> {
    let (input, digit) = digit1(input)?;
    let digit = digit.chars().next().unwrap();
    Ok((input, digit))
}

fn char_alphabetic(input: &str) -> IResult<&str, char> {
    let (input, alphabetic) = alphanumeric1(input)?;
    let alphabetic = alphabetic.chars().next().unwrap();
    Ok((input, alphabetic))
}

fn parse_float(input: &str) -> IResult<&str, Argument> {
    let (input, float_val) = recognize_float(input)?;
    let float_val = float_val.parse::<f64>().unwrap();
    Ok((input, Argument::Float(float_val)))
}

fn recognize_float(input: &str) -> IResult<&str, &str> {
    recognize_float_inner(input).map_err(|_: nom::Err<nom::error::Error<&str>>| {
        nom::Err::Error(Error::new(input, ErrorKind::Float))
    })
}

fn recognize_float_inner(input: &str) -> IResult<&str, &str> {
    recognize_float_prefix(input)
        .and_then(|(i, _)| recognize_float_suffix(i).map(|(i, _)| (i, ())))
        .map(|(i, _)| (i, input))
}

fn recognize_float_prefix(input: &str) -> IResult<&str, &str> {
    alt((
        recognize_float_integer_prefix,
        recognize_float_decimal_prefix,
    ))(input)
}

fn recognize_float_integer_prefix(input: &str) -> IResult<&str, &str> {
    recognize_float_digits(input)
        .and_then(|(i, _)| recognize_float_integer_exponent(i).map(|(i, _)| (i, ())))
}

fn recognize_float_integer_exponent(input: &str) -> IResult<&str, &str> {
    recognize_float_exponent_prefix(input)
        .and_then(|(i, _)| recognize_float_exponent_digits(i).map(|(i, _)| (i, ())))
}

fn recognize_float_decimal_prefix(input: &str) -> IResult<&str, &str> {
    recognize_float_digits(input).and_then(|(i, _)| recognize_float_decimal_suffix(i))
}

fn recognize_float_decimal_suffix(input: &str) -> IResult<&str, &str> {
    recognize_float_exponent_prefix(input)
        .and_then(|(i, _)| recognize_float_decimal_digits(i).map(|(i, _)| (i, ())))
}

fn recognize_float_exponent_prefix(input: &str) -> IResult<&str, &str> {
    alt((tag("e"), tag("E")))(input)
}

fn recognize_float_digits(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_ascii_digit())(input)
}

fn recognize_float_decimal_digits(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c.is_ascii_digit())(input)
}

fn recognize_float_exponent_digits(input: &str) -> IResult<&str, &str> {
    recognize_float_sign(input)
        .and_then(|(i, _)| recognize_float_digits(i))
        .or(recognize_float_digits)(input)
}

fn recognize_float_sign(input: &str) -> IResult<&str, &str> {
    alt((tag("+"), tag("-")))(input)
}


/*
==================================================================================================================================
          ///                       ///
         ///         Loops         ///
        ///                       ///
==================================================================================================================================
*/

fn parse_if_statement(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = tag("IF")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("THEN")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, else_if_conditions) =
        many0(preceded(parse_else_if_condition, parse_g_code_command))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, else_commands) = opt(preceded(tag("ELSE"), many0(parse_g_code_command)))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("ENDIF")(input)?;
    Ok((
        input,
        GCodeCommand::IfStatement(IfStatement {
            condition,
            commands,
            else_if_conditions,
            else_commands,
        }),
    ))
}

fn parse_else_if_condition(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("ELSEIF")(input)?;
    let (input, _) = multispace0(input)?;
    parse_expression(input)
}

fn parse_while_loop(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = tag("WHILE")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("DO")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("ENDWHILE")(input)?;
    Ok((
        input,
        GCodeCommand::WhileLoop(WhileLoop { condition, commands }),
    ))
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
    let gcode = vec![
        "N000 O0001 (This is a comment on code)",
        "N001 M06 T1",
        "N002 G0 G20 G54 X2.00 Y2.00 Z4.00",
        "N003 G01 G43 F20.0 Z-5.00 H01 M08",
        "N004 M106 S255",
        "N005 IF #1 > 0 GOTO N012",
        "N006 ELSE IF #2 == 0 THEN [G1 X50 Y60 Z70]",
        "N007 ELSE [G1 X70 Y80 Z90]",
        "N008 [G1 X10 Y20 Z30 #3 = #3 + 1]",
        "N009 M98 P0100",
        "N010 #1000=100 #2000=X20.45 #3000=Y456.908 #4000=Z345.875",
        "N011 G0 X#1000 Y#2000 Z#3000",
        "N012 IF [#100 == 0] THEN #100 = 1 (Avoid dividing by zero!)",
        "N013 IF [#100 <= 0] THEN #100 = 10",
        "N014 ELSE [#100 >= 0] THEN #100 = -10",
        "N015 WHILE [#3 == 10] DO1",
        "N016 IF [#100 EQ 0] THEN #100 = 1 (Avoid dividing by zero!)",
        "N017 IF [#100 GT 0] THEN #100 = 10",
        "N018 IF [#100 LT 0] THEN #100 = -10",
        "N019 ELSE #100=0",
        "N020 END1",
        "N021 #1=SIN[#2]",
        "N022 #1=COS[#3]",
        "N023 #1=TAN[#4]",
        "N024 #1=ACOS[#5]",
        "N025 #1=ATAN[#6]/[#7]",
        "N026 #1=SQRT=[#8]",
        "N027 #1=ABS[#9]",
        "N028 #1=LN[#10]",
        "N029 #1=EXP[#11] (Exponent base e)",
        "N030 #1=ADP[#12] (Add a decimal point to the end of the number)",
        "N031 #1=ROUND[#13]",
        "N032 #1=FUP[#14]",
        "N033 #1=FIX[#15]",
        "N034 #1=BIN[#16] (Convert from BCD to binary)",
        "N035 #1=BCD[#17] (Convert from binary to BCD)",
        "N036 DPRNT [OUTPUT*TIME:**#3000[60]]",
        "N037 M30",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect::<Vec<String>>();

    parse_and_process_gcode_concurrently(&gcode);


    match parse_g_code_program(code) {
        Ok((_, program)) => println!("{:#?}", program),
        Err(err) => println!("Parsing Error: {:?}", err),
    }
}

let commands: Vec<Command> = gcode
        .lines()
        .filter_map(|line| parse_code(line).ok().map(|(_, command)| command))
        .collect();

    for command in commands {
        println!("{:?}", command);
    }
}

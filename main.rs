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

// Import HashMap Function for storing macro variable data
use std::collections::HashMap;

// Import nom for data parsing
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0},
    combinator::{map, opt},
    multi::many0,
    sequence::{preceded, tuple},
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
struct GCodeProgram(Vec<GCodeCommand>);

#[derive(Debug)]
struct GCommand {
    code: String,
    parameters: HashMap<char, ParameterValue>,
}

#[derive(Debug)]
struct MCommand {
    code: String,
    parameters: HashMap<char, ParameterValue>,
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
    letter: char,
    parameters: HashMap<char, ParameterValue>,
}

/*
==================================================================================================================================
          ///                             ///
         ///         Enumerators         ///
        ///                             ///
==================================================================================================================================
*/

#[derive(Debug)]
enum GCodeCommand {
    GCommand(GCommand),
    MCommand(MCommand),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    VariableCall(VariableCall),
    MacroCall(MacroCall),
}

#[derive(Debug)]
enum Expression {
    Constant(f64),
    Variable(usize),
    BinaryOperation(BinaryOperation),
}

#[derive(Debug)]
enum BinaryOperation {
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Number(f64),
    Variable(usize),
}

#[derive(Debug)]
enum ParameterValue {
    Float(f64),
    Variable(Expression),
}

/*
==================================================================================================================================
          ///                                 ///
         ///         Program Parsing         ///
        ///                                 ///
==================================================================================================================================
*/

// Function to parse a parameter value
fn parse_parameter_value(input: &str) -> IResult<&str, ParameterValue> {
    alt((
        map(parse_float, ParameterValue::Float),
        map(parse_expression, ParameterValue::Variable),
    ))(input)
}

fn parse_g_code_program(input: &str) -> IResult<&str, GCodeProgram> {
    let (input, commands) = many0(parse_g_code_command)(input)?;
    Ok((input, GCodeProgram(commands)))
}

/*  
=================================================================================================================================
          ///                                              ///
         ///         G-Code, Macro, and Variables         ///
        ///                                              ///
=================================================================================================================================
*/

// Function to parse a G-code command
fn parse_g_code_command(input: &str) -> IResult<&str, GCodeCommand> {
    alt((
        parse_g_command,
        parse_m_command,
        parse_if_statement,
        parse_while_loop,
        parse_variable_call,
        parse_macro_call,
    ))(input)
}

fn parse_g_command(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('G')(input)?;
    let (input, code) = digit1(input)?;
    let (input, parameters) = many0(parse_parameter)(input)?;
    Ok((
        input,
        GCodeCommand::GCommand(GCommand {
            code: code.to_string(),
            parameters: parameters.into_iter().collect(),
        }),
    ))
}

// Function to parse an M-code command
fn parse_m_command(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('M')(input)?;
    let (input, code) = digit1(input)?;
    let (input, parameters) = many0(parse_parameter)(input)?;
    Ok((
        input,
        GCodeCommand::MCommand(MCommand {
            code: code.to_string(),
            parameters: parameters.into_iter().collect(),
        }),
    ))
}

fn parse_variable_call(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = char('#')(input)?;
    let (input, variable) = digit1(input)?;
    Ok((input, GCodeCommand::VariableCall(VariableCall {
        variable: variable.parse().unwrap(),
    })))
    }

fn parse_variable_expression(input: &str) -> IResult<&str, Expression> {
        let (input, _) = char('#')(input)?;
        let (input, variable) = digit1(input)?;
        Ok((
            input,
            Expression::Variable(variable.parse().unwrap()),
        ))
}

fn parse_macro_call(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, letter) = char('#')(input)?;
    let (input, parameters) = many0(parse_parameter)(input)?;
    Ok((
        input,
        GCodeCommand::MacroCall(MacroCall {
            letter,
            parameters: parameters.into_iter().collect(),
        }),
    ))
}

// Define a function to parse an individual G-code parameter (X, Y, Z, I, J)
fn parse_parameter(input: &str) -> IResult<&str, (char, ParameterValue)> {
    let (input, _) = multispace0(input)?;
    let (input, parameter) = 
        preceded(char(' '), alt((char('X'), char('Y'), char('Z'), char('I'), char('J'), char('K'), char('L'), char('T'), char('D'), char('S'), char('P'))))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, value) = parse_parameter_value(input)?;
    
    Ok((input, (parameter, value)))
}

/*
==================================================================================================================================
          ///                                  ///
         ///         Logic Statements         ///
        ///                                  ///
==================================================================================================================================
*/

fn parse_if_statement(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = tuple((tag("if"), multispace0))(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, else_if_conditions) = many0(parse_else_if_condition)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, else_commands) = opt(parse_else_commands)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    
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

fn parse_else_if_condition(input: &str) -> IResult<&str, (Expression, Vec<GCodeCommand>)> {
    let (input, _) = tuple((tag("else"), multispace0))(input)?;
    let (input, _) = tag("if")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, (condition, commands)))
}

fn parse_else_commands(input: &str) -> IResult<&str, Vec<GCodeCommand>> {
    let (input, _) = tuple((tag("else"), multispace0))(input)?;
    let (input, _) = char('{')(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, commands))
}

fn parse_while_loop(input: &str) -> IResult<&str, GCodeCommand> {
    let (input, _) = tuple((tag("while"), multispace0))(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, commands) = many0(parse_g_code_command)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((
        input,
        GCodeCommand::WhileLoop(WhileLoop {
            condition,
            commands,
        }),
    ))
}

fn parse_binary_operation(input: &str) -> IResult<&str, Expression> {
    let (input, _) = char('(')(input)?;
    let (input, left) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, operator) = alt((
        char('+'),
        char('-'),
        char('*'),
        char('/'),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, right) = parse_expression(input)?;
    let (input, _) = char(')')(input)?;
    let operation = match operator {
        '+' => BinaryOperation::Add,
        '-' => BinaryOperation::Subtract,
        '*' => BinaryOperation::Multiply,
        '/' => BinaryOperation::Divide,
        _ => unreachable!(),
    };
    
    Ok((input, Expression::BinaryOperation(operation(Box::new(left), Box::new(right)))))
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
        parse_constant_expression,
        parse_variable_expression,
        parse_binary_operation,
    ))(input)
}

fn parse_constant_expression(input: &str) -> IResult<&str, Expression> {
    let (input, constant) = parse_float(input)?;
    Ok((input, Expression::Constant(constant)))
}

fn parse_float(input: &str) -> IResult<&str, f64> {
    let (input, number) = digit1(input)?;
    let (input, _) = opt(tuple((char('.'), digit1)))(input)?;

    Ok((input, number.parse().unwrap()))
}

fn char_alphanumeric(input: &str) -> IResult<&str, char> {
    let (input, char) = nom::character::complete::alphanumeric1(input)?;
    Ok((input, char.chars().next().unwrap()))
}

/*
==================================================================================================================================
          ///                      ///
         ///         Main         ///
        ///                      ///
==================================================================================================================================
*/
fn main() {
    let input = r#"
        G1 X10 Y20 Z30
        M106 S255
        if (#1 > 0) {
            G1 X30 Y40 Z50
        } else if (#2 == 0) {
            G1 X50 Y60 Z70
        } else {
            G1 X70 Y80 Z90
                }
        while (#3 < 10) {
            G1 X10 Y20 Z30
            #3 = #3 + 1
            }
        #A = #1 + #2
        #B = #A * 2
        #C = (#A + #B) / 2
        #D = #C
        M107
        #E = #D
        #F = #E * 2
        N100 G1 X10 Y10
        N101 G1 X20 Y20
    "#;

    let (_, program) = parse_g_code_program(input).unwrap();
    println!("{:#?}", program);
}

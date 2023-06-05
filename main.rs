/*
 * Rust GCode Parser and Interpreter
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




//use core::num::dec2flt::number::Number;
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

use regex::Regex;
use std::fmt;
use std::collections::HashMap;
use std::{process, default};
use std::io::{BufRead, BufReader, Write};
use nom::error::context;
use std::error::Error;
use std::fs;
use std::env;
use std::path::Path;
use std::str::FromStr;

/*
==================================================================================================================================
          ///                             ///
         ///         Enumerators         ///
        ///                             ///
==================================================================================================================================
*/

#[derive(Debug)]
enum Command {
    GCode(String),
    MCode(String),
    TCode(String),
    FCode(String),
    SCode(String),
    IfStatement(String),
    WhileLoop(String),
    Unknown(String),
}

#[derive(Debug)]
enum Expression {
    Constant(i32),
    Variable(String),
    BinaryOperation(Box<BinaryOperation>),
    TrigonometricOperation(Box<TrigonometricOperation>),
}

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
enum TrigonometricFunction {
    Sin,
    Cos,
    Tan,
    Acos,
    Atan,
    Sqrt,
    Abs,
    Ln,
    Exp,
    Adp,
    Round,
    Fup,
    Fix,
    Bin,
    Bcd,
}


/*
==================================================================================================================================
          ///                            ///
         ///         Structures         ///
        ///                            ///
==================================================================================================================================
*/



#[derive(Debug)]
struct IfStatement {
    condition: Expression,
    line_commands: Vec<String>,
    else_if_conditions: Vec<(Expression, Vec<String>)>,
    else_commands: Option<Vec<String>>,
}

#[derive(Debug)]
struct WhileLoop {
    condition: Expression,
    line_commands: Vec<String>,
}

#[derive(Debug)]
struct BinaryOperation {
    left: Box<Expression>,
    operator: BinaryOperator,
    right: Box<Expression>,
}

#[derive(Debug)]
struct TrigonometricOperation {
    function: TrigonometricFunction,
    argument: Box<Expression>,
}

#[derive(Default)]
struct ExecutionContext {
    // Implement the execution context structure
    // to hold the variable values and provide methods
    // for accessing and updating variable values
    modal_state: ModalState,
    variables: Variables,
}

#[derive(Default)]
struct ModalState {
    // Add your state variables here
    feed_rate: f64,
    spindle_speed: f64,
    // Add more state variables as needed
}

#[derive(Default)]
struct Variables {
    // Implement the variables storage and operations here
    // Example: map of variable names to their values
    // Assuming variables are stored as key-value pairs of strings
    variables: HashMap<String, f64>
}

/*
==================================================================================================================================
          ///                    Implaments                              ///
         ///                         to                                 ///
        ///            Interpret and Parse Command Line                ///
==================================================================================================================================
*/

#[derive(Debug)]
struct CustomError(String);

impl std::fmt::Display for CustomError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CustomError {}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Implement the formatting logic for each command variant
        match self {
            // Implement formatting for each variant
        }
    }
}


impl ModalState {
    pub fn new() -> Self {
        // Initialize the state variables to their default values
        ModalState {
            feed_rate: 0.0,
            spindle_speed: 0.0,
            // Initialize more state variables here
        }
    }

    pub fn set_feed_rate(&mut self, rate: f64) {
        // Set the feed rate
        self.feed_rate = rate;
    }

    pub fn set_spindle_speed(&mut self, speed: f64) {
        // Set the spindle speed
        self.spindle_speed = speed;
    }

    // Add more methods for setting and retrieving other state variables
}



impl Variables {
    pub fn new() -> Self {
        // Initialize the variable storage
        Variables {
            variables: HashMap::new(),
        }
    }


    fn set_variable(&mut self, variable_name: String, value: f64) {
        // Set the value of a variable
        self.variables.insert(variable_name, value);
    }

    fn get_variable_value(&self, variable_name: &str) -> Option<f64> {
        // Get the value of a variable
        self.variables.get(variable_name).cloned()
    }

    // Add more methods for manipulating variables
}



impl ExecutionContext {
    
    
    fn evaluate_condition(&self, condition: &str) -> bool {
        // Implement the evaluation of conditions based on the G-code specification
        // and the context (variable values) available in the `context` parameter
        // Return true if the condition is true, otherwise false
        // Assume that the condition is a simple equality check between two variables
        let parts: Vec<&str> = condition.split('=').map(|s| s.trim()).collect();
        if parts.len() < 2 {
            eprintln!("Invalid condition format: {}", condition);
            return false;
        }
    
        let variable1 = parts[0];
        let variable2 = parts[1];
    
        // Assuming both variables are strings
        let value1 = match self.variables.get_variable_value(variable1) {
            Some(val) => val,
            None => {
                eprintln!("Variable not found: {}", variable1);
                return false;
            }
        };
    
        let value2 = match self.variables.get_variable_value(variable2) {
            Some(val) => val,
            None => {
                eprintln!("Variable not found: {}", variable2);
                return false;
            }
        };
    
        value1 == value2
    }

    fn execute_commands(&mut self, command: Result<Command, Box<dyn std::error::Error>>) -> Result<(), Box<dyn std::error::Error>> {
        // Extract the lines of the command
        let lines = match &command {
            Ok(cmd) => cmd.to_string().lines().map(String::from).collect::<Vec<String>>(),
            Err(err) => return Err(Box::new(err.clone())),
        };
        
        // Process each command line
        for line in lines {
            // Split the line into individual command tokens
            let tokens: Vec<&str> = line.trim().split_whitespace().collect();
    
            // Check if the line is a variable assignment
            if tokens.len() >= 3 && tokens[1] == "=" {
                let variable = tokens[0];
                let value: f64 = tokens[2].parse().unwrap();
    
                // Set the variable in the context's variables
                self.variables.set_variable(variable.to_string(), value);
            }
    
            // Get the command and its arguments
            if let Some((outer_command, args)) = tokens.split_first() {
                match outer_command {
                    &"GCode" | &"MCode" | &"TCode" | &"FCode" | &"SCode" | &"IfStatement" | &"WhileLoop" => {
                        let inner_lines: Vec<&str> = args.iter().flat_map(|&line| line.split('\n')).filter(|&line| !line.trim().is_empty()).collect();
                        for &inner_line in &inner_lines {
                            let trimmed_line = inner_line.trim();
                            if trimmed_line.is_empty() {
                                continue;
                            }
    
                            let parsed_command = self.parse_command(trimmed_line);
                            self.execute_commands(parsed_command)?;
                        }
                    }
                    &"Unknown" => {
                        return Err(Box::new(CustomError(format!("Unknown command: {:?}", command)))) as Result<(), Box<dyn std::error::Error>>;
                    }
                    &"PRINT" => {
                        // Execute the PRINT command
                        if let Some((variable, _)) = args.split_first() {
                            if let Some(value) = self.variables.get_variable_value(variable) {
                                println!("{} = {}", variable, value);
                            }
                        }
                    }
                    // Add more commands as needed
                    //_ => {
                    //    println!("Unknown command: {}", command);
                    //}
                }
            }
        }
        // Return the executed command
        Ok(())
    }
    
    

    
    fn new() -> Self {
        // Implement the initialization of the execution context
        // with default values for variables
        ExecutionContext {
            modal_state: ModalState::new(),
            variables: Variables::new(),

        }
    }

    fn get_variable(&self, variable_name: &str) -> Option<&f64> {
        // Implement the method to retrieve the value of a variable
        // from the execution context
        self.variables.variables.get(variable_name)
    }

    fn set_variable(&mut self, variable_name: &str, value: f64) {
        // Implement the method to update the value of a variable
        // in the execution context
        self.variables.variables.insert(variable_name.to_string(), value);
    }

    fn parse_command(&mut self, line: &str) -> Result<Command, Box<dyn std::error::Error>> {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        
        let command = match tokens[0] {
            "G" => Command::GCode(line.to_string()),
            "M" => Command::MCode(line.to_string()),
            "T" => Command::TCode(line.to_string()),
            "F" => Command::FCode(line.to_string()),
            "S" => Command::SCode(line.to_string()),
            "IF" => Command::IfStatement(line.to_string()),
            "WHILE" => Command::WhileLoop(line.to_string()),
            _ => Command::Unknown(line.to_string()),
        };
        Ok(command)
    }

    fn interpret_command(&mut self, command: Command) {
        let re_g = Regex::new(r"^G([0-9]+|\d{2})$").unwrap();
        let re_m = Regex::new(r"^M([0-9]+|\d{2})$").unwrap();

        
        match command {
            Command::GCode(g) => {
                // Handle GCode command
                let re_g0 = Regex::new(r"^G[0]0*$").unwrap();
                let re_g1 = Regex::new(r"^G[0]1*$").unwrap();
                let re_g2 = Regex::new(r"^G[0]2*$").unwrap();
                let re_g3 = Regex::new(r"^G[0]3*$").unwrap();
                let re_g4 = Regex::new(r"^G[0]4*$").unwrap();
                let re_g5 = Regex::new(r"^G[0]5*$").unwrap();
                let re_g6 = Regex::new(r"^G[0]6*$").unwrap();
                let re_g7 = Regex::new(r"^G[0]7*$").unwrap();
                let re_g8 = Regex::new(r"^G[0]8*$").unwrap();
                let re_g9 = Regex::new(r"^G[0]9*$").unwrap();

                if re_g0.is_match(&g) {
                    // Handle G0 or G00 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G0 or G00 command: {:?}", coordinates);
                } else if re_g1.is_match(&g) {
                    // Handle G01 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G1 or G01 command: {:?}", coordinates);
                } else if re_g2.is_match(&g) {
                    // Handle G2 or G02 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G2 or G02 command: {:?}", coordinates);
                } else if re_g3.is_match(&g) {
                    // Handle G3 or G03 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G3 or G03 command: {:?}", coordinates);
                } else if re_g4.is_match(&g) {
                    // Handle G4 or G04 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G4 or G04 command: {:?}", coordinates);
                } else if re_g5.is_match(&g) {
                    // Handle G5 or G05 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G5 or G05 command: {:?}", coordinates);
                } else if re_g6.is_match(&g) {
                    // Handle G6 or G06 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G6 or G06 command: {:?}", coordinates);
                } else if re_g7.is_match(&g) {
                    // Handle G7 or G07 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G7 or G07 command: {:?}", coordinates);
                } else if re_g8.is_match(&g) {
                    // Handle G8 or G08 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G8 or G08 command: {:?}", coordinates);
                } else if re_g9.is_match(&g) {
                    // Handle G9 or G09 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G9 or G09 command: {:?}", coordinates);
                } else if g.starts_with("G10") {
                    // Handle G10 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G10 command: {:?}", coordinates);
                } else {
                    // Handle other GCode commands
                    println!("Interpreting GCode command: {:?}", g);
                }
            }
            Command::MCode(m) => {
                // Handle MCode command
                let re_m0 = Regex::new(r"^M[0]0*$").unwrap();
                let re_m1 = Regex::new(r"^M[0]1*$").unwrap();
                let re_m2 = Regex::new(r"^M[0]2*$").unwrap();
                let re_m3 = Regex::new(r"^M[0]3*$").unwrap();
                let re_m4 = Regex::new(r"^M[0]4*$").unwrap();
                let re_m5 = Regex::new(r"^M[0]5*$").unwrap();
                let re_m6 = Regex::new(r"^M[0]6*$").unwrap();
                let re_m7 = Regex::new(r"^M[0]7*$").unwrap();
                let re_m8 = Regex::new(r"^M[0]8*$").unwrap();
                let re_m9 = Regex::new(r"^M[0]9*$").unwrap();

                if re_m0.is_match(&m) {
                    // Handle M0 or M00 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M0 or M00 command: {:?}", coordinates);
                } else if re_m1.is_match(&m) {
                    // Handle M1 or M01 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M1 or M01 command: {:?}", coordinates);
                } else if re_m2.is_match(&m) {
                    // Handle M2 or M02 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M2 or M02 command: {:?}", coordinates);
                } else if re_m3.is_match(&m) {
                    // Handle M3 or M03 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M3 or M03 command: {:?}", coordinates);
                } else if re_m4.is_match(&m) {
                    // Handle M4 or M04 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M4 or M04 command: {:?}", coordinates);
                } else if re_m5.is_match(&m) {
                    // Handle M5 or M05 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M5 or M05 command: {:?}", coordinates);
                } else if re_m6.is_match(&m) {
                    // Handle M6 or M06 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M6 or M06 command: {:?}", coordinates);
                } else if re_m7.is_match(&m) {
                    // Handle M7 or M07 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M7 or M07 command: {:?}", coordinates);
                } else if re_m8.is_match(&m) {
                    // Handle M8 or M08 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M8 or M08 command: {:?}", coordinates);
                } else if re_m9.is_match(&m) {
                    // Handle M9 or M09 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M9 or M09 command: {:?}", coordinates);
                } else if m.starts_with("M10") {
                    // Handle M10 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M10 command: {:?}", coordinates);
                } else {
                    // Handle other GCode commands
                    println!("Interpreting MCode command: {:?}", m);
                }
            }
            Command::TCode(t) => {
                // Handle TCode command
                println!("Interpreting TCode command: {:?}", t);
            }
            Command::FCode(f) => {
                // Handle FCode command
                println!("Interpreting FCode command: {:?}", f);
            }
            Command::SCode(s) => {
                // Handle SCode command
                println!("Interpreting SCode command: {:?}", s);
            }
            Command::IfStatement(if_statement) => {
                // Handle IfStatement command
                println!("Interpreting IfStatement: {:?}", if_statement);
            }
            Command::WhileLoop(while_loop) => {
                // Handle WhileLoop command
                println!("Interpreting WhileLoop: {:?}", while_loop);
            }
            Command::Unknown(unknown) => {
                // Handle default command
                println!("Interpreting Unknown command");
            }
        }
    }

    pub fn gcode(&self, file_name: &str) -> Result<(), Box<dyn Error>> {
        // Implement your gcode function logic here
        // Use the provided file name to read and process the input file
        let re_goto = Regex::new(r"GOTO N(\d+)").unwrap();
        let re_if_else = Regex::new(r"(IF|ELSE|ELSE IF) ([^[]+)\[(.+)\]").unwrap();
        let re_while_end = Regex::new(r"(WHILE|END)(\d+)").unwrap();
            
        let mut context = ExecutionContext::new();
        
        // Read the file contents into a string
        let file_contents = fs::read_to_string(file_name)?;
    
        let lines: Vec<&str> = file_contents.split('\n').collect();
    
        let mut line_num = 0;
        let nested_level = 0;
        let mut loop_stack = Vec::new();
    
        while line_num < lines.len() {
            let line = lines[line_num].trim();
            if line.is_empty() {
                line_num += 1;
                continue;
            }
    
            if let Some(caps) = re_goto.captures(line) {
                // Handle GOTO command
                let target_line = caps[1].parse::<usize>().unwrap();
                line_num = target_line;
                continue;
            }
    
            if let Some(caps) = re_if_else.captures(line) {
                // Handle IF/ELSE/ELSE IF statements
                let statement_type = &caps[1];
                let condition = &caps[2];
    
                match statement_type {
                    "IF" => {
                        // Evaluate the condition and execute the corresponding commands
                        let condition_result = Self::evaluate_condition(&context, condition);
                        if condition_result {
                                let mut new_line_num = line_num;
                                new_line_num += 1;
                                line_num = new_line_num;
                            
                                
                            continue;
                        }
                    }
                    "ELSE IF" => {
                        // Evaluate the condition and execute the corresponding commands if the previous conditions were false
                        let condition_result = Self::evaluate_condition(&context, condition);
                        if condition_result && nested_level == 0 {
                                let mut new_line_num = line_num;
                                new_line_num += 1;
                                line_num = new_line_num;
                            
    
                            continue;
                        }
                    }
                    "ELSE" => {
                        // Execute the corresponding commands if the previous conditions were false
                        if nested_level == 0 {
                                let mut new_line_num = line_num;
                                new_line_num += 1;
                                line_num = new_line_num;
                                if nested_level == 0 {
                                    // Put your code here
                                }
                        }
                    }
                    _ => {}
                }
            }
    
            if let Some(caps) = re_while_end.captures(line) {
                // Handle WHILE/END statements
                let statement_type = &caps[1];
                let loop_id = caps[2].parse::<usize>().unwrap();
    
                match statement_type {
                    "WHILE" => {
                        // Evaluate the condition and push the loop onto the stack if it's true
                        let condition = lines[line_num + 1].trim_start_matches("IF").trim();
                        let condition_result = Self::evaluate_condition(&context, condition);
                        if condition_result {
                            loop_stack.push(loop_id);
                        } else {
                            // Skip lines until the corresponding END statement
                            let mut nested_end_count = 0;
                            while line_num < lines.len() - 1 {
                                line_num += 1;
                                if lines[line_num].trim() == format!("END{}", loop_id) {
                                    if nested_end_count == 0 {
                                        break;
                                    } else {
                                        nested_end_count -= 1;
                                    }
                                } else if re_while_end.is_match(lines[line_num].trim()) {
                                    nested_end_count += 1;
                                }
                            }
                        }
                    }
                    "END" => {
                        // Pop the loop from the stack if its corresponding END statement is reached
                        if let Some(last_loop) = loop_stack.pop() {
                            if last_loop != loop_id {
                                panic!("Mismatched loop statements");
                            }
                        } else {
                            panic!("Unexpected END statement");
                        }
                    }
                    _ => {}
                }
            }
    
            // Process other G-code commands here
    
            line_num += 1;
        }
    
        Ok(()) // Return Ok(()) if the gcode function executes successfully
    }
    
}





/*
==================================================================================================================================
          ///                                                                ///
         ///         Main with Parsing, Logic, Variables, and Loops         ///
        ///                                                                ///
==================================================================================================================================
*/

fn main() {
    let mut context = ExecutionContext::default();

    let mut variables = Variables::new();

    // Read command-line arguments
    let args: Vec<String> = env::args().collect();

    let mut line_num = 0;
   
    // Check if the input file name is provided
    if args.len() < 2 {
        eprintln!("Please provide the input file name.");
        process::exit(1);
    }

    // Get the input file name from the command-line argument
    let input_file = &args[1];

    // Check if the input file has a valid extension
    let input_path = Path::new(input_file);
    let extension = input_path.extension().and_then(|ext| ext.to_str());

    
    if extension != Some("txt") && extension != Some("nc") {
        eprintln!("Invalid input file format. Please provide a .txt or .nc file.");
        process::exit(1);
    }

    let command_result: Result<Command, Box<dyn std::error::Error>> = context.parse_command(&input_file);

    match command_result {
        Ok(command) => {
            if let Err(err) = context.execute_commands(Ok(command)) {
                eprintln!("Error: {}", err);
                // Handle the error as needed
            }
        }
        Err(err) => {
            let error_message = format!("Unknown command: {:?}", err);
            let boxed_error: Box<dyn std::error::Error> = Box::new(CustomError(error_message));
            eprintln!("Error: {}", boxed_error);
            // Handle the error as needed
        }
    }
}

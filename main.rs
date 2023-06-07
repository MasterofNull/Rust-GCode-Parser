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
use std::collections::HashMap;
use std::fmt;
use std::env;
use std::fs;
use std::path::Path;
use std::process;
//use nom::error::context;


/*
==================================================================================================================================
          ///                             ///
         ///         Enumerators         ///
        ///                             ///
==================================================================================================================================
*/
 
 #[derive(Debug, Clone)]
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
     Constant(f64),
     Variable(String),
     BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
     TrigonometricOperation(Box<Expression>, TrigonometricOperation),
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

#[derive(Debug, Clone)]
enum CommandError {
    UnknownCommand(String),
    // Add more specific error variants as needed
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
    commands: Commands,
}

#[derive(Default)]
struct ModalState {
    // Add your state variables here
    feed_rate: f64,
    spindle_speed: f64,
    feed_type: String,
    // Add more state variables as needed
}

#[derive(Default)]
struct Variables {
    // Implement the variables storage and operations here
    // Example: map of variable names to their values
    // Assuming variables are stored as key-value pairs of strings
    variables: HashMap<String, f64>,
    var1: HashMap<String, f64>,
    var2: HashMap<String, f64>,
}

#[derive(Default)]
struct Commands {
    commands: Vec<String>,
}

/*
==================================================================================================================================
          ///                    Implaments                              ///
         ///                         to                                 ///
        ///            Interpret and Parse Command Line                ///
==================================================================================================================================
*/

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CommandError::UnknownCommand(command) => write!(f, "Unknown command: {}", command),
            // Handle other error variants here
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "Add"),
            BinaryOperator::Subtract => write!(f, "Subtract"),
            BinaryOperator::Multiply => write!(f, "Multiply"),
            BinaryOperator::Divide => write!(f, "Divide"),
        }
    }
}

impl fmt::Display for TrigonometricFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TrigonometricFunction::Sin => write!(f, "Sin"),
            TrigonometricFunction::Cos => write!(f, "Cos"),
            TrigonometricFunction::Tan => write!(f, "Tan"),
            TrigonometricFunction::Acos => write!(f, "Acos"),
            TrigonometricFunction::Atan => write!(f, "Atan"),
            TrigonometricFunction::Sqrt => write!(f, "Sqrt"),
            TrigonometricFunction::Abs => write!(f, "Abs"),
            TrigonometricFunction::Ln => write!(f, "Ln"),
            TrigonometricFunction::Exp => write!(f, "Exp"),
            TrigonometricFunction::Adp => write!(f, "Adp"),
            TrigonometricFunction::Round => write!(f, "Round"),
            TrigonometricFunction::Fup => write!(f, "Fup"),
            TrigonometricFunction::Fix => write!(f, "Fix"),
            TrigonometricFunction::Bin => write!(f, "Bin"),
            TrigonometricFunction::Bcd => write!(f, "Bcd"),
        }
    }
}


impl std::error::Error for CommandError {}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Implement the formatting logic for each command variant
        match self {
            Command::GCode(field1) => write!(f, "GCode({})", field1),
            Command::MCode(field1) => write!(f, "MCode({})", field1),
            Command::TCode(field1) => write!(f, "TCode({})", field1),
            Command::SCode(field1) => write!(f, "SCode({})", field1),
            Command::FCode(field1) => write!(f, "FCode({})", field1),
            Command::Unknown(field1) => write!(f, "Unknown({})", field1),
            Command::IfStatement(field1) => write!(f, "IfStatement({})", field1),
            Command::WhileLoop(field1) => write!(f, "WhileLoop({})", field1),
        }
    }
}

impl ModalState {
    pub fn new() -> Self {
        // Initialize the state variables to their default values
        ModalState {
            feed_rate: 0.0,
            spindle_speed: 0.0,
            feed_type: "G00".to_string(),
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
        let mut variables = Variables {
            var1: HashMap::new(),
            var2: HashMap::new(),
        };
        
        variables.var1.insert(String::from("default_var1"), 0.0);
        variables.var2.insert(String::from("default_var2"), 0.0);
    }


    fn set_variable(&mut self, variable_name: String, value: f64) {
        // Set the value of a variable
        self.variables.insert(variable_name, value);
    }

    fn get_variable_value(&self, variable_name: &str) -> Option<f64> {
        // Get the value of a variable
        self.variables.get(variable_name).copied()
    }

    // Add more methods for manipulating variables
}



impl ExecutionContext {
    fn evaluate(&self, expression: &Expression) -> f64 {
        match expression {
            Expression::Constant(value) => *value,
            Expression::Variable(name) => {
                *self.get_variable_value(name).unwrap_or(&0.0)
            }
            Expression::BinaryOperation(left, operator, right) => {
                let left_value = self.evaluate(left);
                let right_value = self.evaluate(right);
                match operator {
                    BinaryOperator::Add => left_value + right_value,
                    BinaryOperator::Subtract => left_value - right_value,
                    BinaryOperator::Multiply => left_value * right_value,
                    BinaryOperator::Divide => left_value / right_value,
                }
            }
            Expression::TrigonometricOperation(argument, function) => {
                let argument_value = self.evaluate(argument);
                match function.function {
                    TrigonometricFunction::Sin => argument_value.sin(),
                    TrigonometricFunction::Cos => argument_value.cos(),
                    TrigonometricFunction::Tan => argument_value.tan(),
                    TrigonometricFunction::Acos => argument_value.acos(),
                    TrigonometricFunction::Atan => argument_value.atan(),
                    TrigonometricFunction::Sqrt => argument_value.sqrt(),
                    TrigonometricFunction::Abs => argument_value.abs(),
                    TrigonometricFunction::Ln => argument_value.ln(),
                    TrigonometricFunction::Exp => argument_value.exp(),
                    TrigonometricFunction::Adp => unimplemented!(),
                    TrigonometricFunction::Round => unimplemented!(),
                    TrigonometricFunction::Fup => unimplemented!(),
                    TrigonometricFunction::Fix => unimplemented!(),
                    TrigonometricFunction::Bin => unimplemented!(),
                    TrigonometricFunction::Bcd => unimplemented!(),
                }
            }
        }
    }


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
        let value1 = match self.get_variable_value(variable1) {
            Some(val) => val,
            None => {
                eprintln!("Variable not found: {}", variable1);
                return false;
            }
        };
    
        let value2 = match self.get_variable_value(variable2) {
            Some(val) => val,
            None => {
                eprintln!("Variable not found: {}", variable2);
                return false;
            }
        };
    
        value1 == value2
    }


    fn execute_commands(&mut self, command: Result<Command, CommandError>) -> Result<Command, CommandError> {
        match command {
            Ok(cmd) => {
                let cloned_cmd = cmd.clone(); // Clone a reference to cmd
    
                match cmd {
                    Command::GCode(g) | Command::MCode(g) | Command::TCode(g) | Command::FCode(g) | Command::SCode(g) | Command::IfStatement(g) | Command::WhileLoop(g) => {
                        let lines: Vec<&str> = g.lines().map(str::trim).collect();
                        let mut last_executed_cmd = cloned_cmd;
    
                        for line in lines {
                            if line.is_empty() {
                                continue;
                            }
    
                            let parsed_command = ExecutionContext::parsed_gcode(line)?;
                            last_executed_cmd = self.execute_commands(Ok(parsed_command))?;
                        }
    
                        Ok(last_executed_cmd)
                    }
                    Command::Unknown(_) => {
                        Err(CommandError::UnknownCommand(format!("{:?}", cloned_cmd)))
                    }
                }
            }
            Err(err) => Err(err),
        }
    }
    
    fn new() -> Self {
        let mut variables = Variables {
            var1: HashMap::new(),
            var2: HashMap::new(),
        };
        
        variables.var1.insert(String::from("default_var1"), 0.0);
        variables.var2.insert(String::from("default_var2"), 0.0);
        // Implement the initialization of the execution context
        // with default values for variables
        ExecutionContext {
            modal_state: ModalState::new(),
            variables: Variables::new(),
            commands: Commands::default()

        }
    }

    fn get_variable_value(&self, variable_name: &str) -> Option<&f64> {
        // Implement the method to retrieve the value of a variable
        // from the execution context
        self.variables.variables.get(variable_name)
    }

    fn set_variable(&mut self, variable_name: &str, value: f64) {
        // Implement the method to update the value of a variable
        // in the execution context
        self.variables.variables.insert(variable_name.to_string(), value);
    }

/*
==================================================================================================================================
            ///                                            ///                   
           ///               Code Handlers                ///
          ///                                            ///
==================================================================================================================================
*/

    
// Helper methods for handling each command type
    fn handle_g0_g00(&mut self, g: &str) {
        // Process the coordinates...
        println!("Interpreting G0 or G00 command: {:?}", g);
    }
    
    fn handle_g1_g01(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }
        
    fn handle_g2_g02(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g3_g03(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g4_g04(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g5_g05(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g6_g06(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g7_g07(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g8_g08(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g9_g09(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    fn handle_g10(&mut self, coordinates: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", coordinates);
    }

    // Implement other GCode handler methods...
    
    fn handle_other_gcode(&self, g: &str) {
        println!("Interpreting GCode command: {:?}", g);
    }
    
    // Implement other MCode handler methods...
    
    fn handle_m0_m00(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G0 or G00 command: {:?}", m);
    }
    
    fn handle_m1_m01(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }
        
    fn handle_m2_m02(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m3_m03(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m4_m04(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m5_m05(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m6_m06(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m7_m07(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m8_m08(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m9_m09(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

    fn handle_m10(&mut self, m: &str) {
        // Process the coordinates...
        println!("Interpreting G1 or G01 command: {:?}", m);
    }

        
    fn handle_other_mcode(&self, m: &str) {
        println!("Interpreting MCode command: {:?}", m);
    }
    
    // Implement other command handler methods...
    
    fn handle_tcode(&self, t: &str) {
        println!("Interpreting TCode command: {:?}", t);
    }
    
    fn handle_fcode(&self, f: &str) {
        println!("Interpreting FCode command: {:?}", f);
    }
    
    fn handle_scode(&self, s: &str) {
        println!("Interpreting SCode command: {:?}", s);
    }
    
    fn handle_if_statement(
        &mut self,
        statement_type: &str,
        context: ExecutionContext,
        line_num: usize,
        loop_stack: Vec<usize>,
        condition: &str
    ) {
        // Evaluate the condition and execute the corresponding commands
        let condition_result = context.evaluate_condition(condition);
        if condition_result {
            let mut new_line_num = line_num;
            new_line_num += 1;
            line_num = new_line_num;
            return;
        }
        println!("Interpreting IfStatement: {:?}", statement_type);
    }
    
    fn handle_else_if_statement(
        &mut self,
        statement_type: &str,
        context: ExecutionContext,
        line_num: usize,
        loop_stack: Vec<usize>,
        condition: &str
    ) {
        // Evaluate the condition and execute the corresponding commands if the previous conditions were false
        let condition_result = Self::evaluate_condition(&context, condition);
        if condition_result && loop_stack.is_empty() {
            let mut new_line_num = line_num;
            new_line_num += 1;
            line_num = new_line_num;
            return;
        }
        println!("Interpreting IfElseStatement: {:?}", statement_type);
    }
    
    fn handle_else_statement(
        &mut self,
        statement_type: &str,
        context: ExecutionContext,
        line_num: usize,
        loop_stack: Vec<usize>,
        condition: &str
    ) {
        // Execute the corresponding commands if the previous conditions were false              
        if loop_stack.is_empty() {
            let mut new_line_num = line_num;
            new_line_num += 1;
            line_num = new_line_num;
            if loop_stack.is_empty() {
                // Put your code here
            }
        }
        println!("Interpreting ElseStatement: {:?}", statement_type);
    }
    
    fn handle_while_loop(
        &self,
        context: ExecutionContext,
        line_num: usize,
        loop_stack: Vec<usize>,
        statement_type: &str,
        loop_id: usize
    ) {
        // Evaluate the condition and push the loop onto the stack if it's true
        let condition = lines[line_num + 1].trim_start_matches("IF").trim();
        let condition_result = Self::evaluate_condition(&context, condition);
        if condition_result {
            loop_stack.push(loop_id);
            return;
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
        println!("Interpreting WhileLoop: {:?}", statement_type);
    }
    
    fn handle_while_loop_end(
        &self,
        context: ExecutionContext,
        line_num: usize,
        loop_stack: Vec<usize>,
        statement_type: &str,
        loop_id: usize
    ) {
        // Pop the loop from the stack if its corresponding END statement is reached
        if let Some(last_loop) = loop_stack.pop() {
            if last_loop != loop_id {
                panic!("Mismatched loop statements");
            }
        } else {
            panic!("Unexpected END statement");
        }
        let mut new_line_num = line_num;
        new_line_num += 1;
        line_num = new_line_num;
        println!("Interpreting WhileLoopEnd: {:?}", statement_type);
    }

    fn handle_unknown_command(&self) {
        println!("Interpreting Unknown command");
    }
    

/*
==================================================================================================================================
          ///                    Implaments                              ///
         ///                         to                                 ///
        ///            Interpret and Parse Command Line                ///
==================================================================================================================================
*/

    pub fn parsed_gcode(value: &str) -> Result<Command, CommandError> {
        let re_goto = Regex::new(r"GOTO N(\d+)").unwrap();
        let re_if_else = Regex::new(r"(IF|ELSE IF) ([^[]+)\[(.+)\]").unwrap();
        let re_while_end = Regex::new(r"(WHILE|END)(\d+)").unwrap();

        let mut context = ExecutionContext::new();

        let lines: Vec<&str> = value.lines().map(str::trim).collect();

        let mut line_num = 0;
        let mut loop_stack: Vec<usize> = Vec::new();
        let mut result_command = Command::Unknown(String::from(value));

        while line_num < lines.len() {
            let line = lines[line_num];
            if line.is_empty() {
                line_num += 1;
                continue;
            }

            if let Some(caps) = re_goto.captures(line) {
                let target_line = caps[1].parse::<usize>().unwrap();
                line_num = target_line;
                continue;
            }

            let command = match line.chars().next() {
                Some('G') => Command::GCode(line[1..].to_string()),
                Some('M') => Command::MCode(line[1..].to_string()),
                Some('T') => Command::TCode(line[1..].to_string()),
                Some('F') => Command::FCode(line[1..].to_string()),
                Some('S') => Command::SCode(line[1..].to_string()),
                Some('I') => Command::IfStatement(line.to_string()),
                Some('W') => Command::WhileLoop(line.to_string()),
                _ => Command::Unknown(line.to_string()),
            };

            match command {
                Command::GCode(g) => {
                    // Handle GCode command
                    match &g[..2] {
                        "0" => context.handle_g0_g00(&g[2..]),
                        "1" => context.handle_g1_g01(&g[2..]),
                        "2" => context.handle_g2_g02(&g[2..]),
                        "3" => context.handle_g3_g03(&g[2..]),
                        "4" => context.handle_g4_g04(&g[2..]),
                        "5" => context.handle_g5_g05(&g[2..]),
                        "6" => context.handle_g6_g06(&g[2..]),
                        "7" => context.handle_g7_g07(&g[2..]),
                        "8" => context.handle_g8_g08(&g[2..]),
                        "9" => context.handle_g9_g09(&g[2..]),
                        "10" => context.handle_g10(&g[3..]),
                        _ => context.handle_other_gcode(&g),
                    }
                    result_command = Command::GCode(g.clone());
                    break;
                }
                Command::MCode(m) => {
                    // Handle MCode command
                    match &m[..2] {
                        "0" => context.handle_m0_m00(&m[2..]),
                        "1" => context.handle_m1_m01(&m[2..]),
                        "2" => context.handle_m2_m02(&m[2..]),
                        "3" => context.handle_m3_m03(&m[2..]),
                        "4" => context.handle_m4_m04(&m[2..]),
                        "5" => context.handle_m5_m05(&m[2..]),
                        "6" => context.handle_m6_m06(&m[2..]),
                        "7" => context.handle_m7_m07(&m[2..]),
                        "8" => context.handle_m8_m08(&m[2..]),
                        "9" => context.handle_m9_m09(&m[2..]),
                        "10" => context.handle_m10(&m[3..]),
                        _ => context.handle_other_mcode(&m),
                    }
                    result_command = Command::MCode(m.clone());
                    break;
                }
                Command::TCode(t) => {
                    // Handle TCode command
                    context.handle_tcode(&t);
                }
                Command::FCode(f) => {
                    // Handle FCode command
                    context.handle_fcode(&f);
                }
                Command::SCode(s) => {
                    // Handle SCode command
                    context.handle_scode(&s);
                }
                Command::Unknown(_) => {
                    // Handle default command
                    context.handle_unknown_command();
                }
                Command::IfStatement(if_statement) => {
                    // Handle IF statement
                    if let Some(caps) = re_if_else.captures(line) {
                        // Handle IF/ELSE/ELSE IF statements
                        let statement_type = &caps[1];
                        let condition = &caps[2];
                        match statement_type {
                            "IF" => context.handle_if_statement(statement_type, context, line_num, loop_stack, condition),
                            "ELSE IF" => context.handle_else_if_statement(statement_type, context, line_num, loop_stack, condition),    
                            "ELSE" => context.handle_else_statement(statement_type, context, line_num, loop_stack, condition),
                            _ => context.handle_unknown_command(),
                        }
                    }
                    result_command = Command::IfStatement(if_statement.clone());
                    break;
                }
                Command::WhileLoop(while_loop) => {
                    // Handle WHILE loop
                    if let Some(caps) = re_while_end.captures(line) {
                        // Handle WHILE/END statements
                        let statement_type = &caps[1];
                        let loop_id = caps[2].parse::<usize>().unwrap();

                        match statement_type {
                            "WHILE" => context.handle_while_loop(context, line_num, loop_stack, statement_type, loop_id),
                            "END" => context.handle_while_loop_end(context, line_num, loop_stack, statement_type, loop_id),   
                            _ => context.handle_unknown_command(),
                        }
                    }
                    result_command = Command::WhileLoop(while_loop.clone());
                    break;
                }
            }

            line_num += 1;
        }

        Ok(result_command)
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

    // Read command-line arguments
    let args: Vec<String> = env::args().collect();

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

    // Read the file contents into a string
    let file_contents = fs::read_to_string(input_file).unwrap();

    // Call the gcode function to process the file contents
    if let Err(err) = ExecutionContext::parsed_gcode(&file_contents) {
        eprintln!("Error: {}", err);
    }
}

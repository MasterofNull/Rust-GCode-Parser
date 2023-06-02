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
    Default,
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

struct ExecutionContext {
    // Implement the execution context structure
    // to hold the variable values and provide methods
    // for accessing and updating variable values
    modal_state: ModalState,
    variables: Variables,
}
struct ModalState {
    // Implement the modal state variables and operations here
}

struct Variables {
    // Implement the variables storage and operations here
    // Example: map of variable names to their values
}

/*
==================================================================================================================================
          ///                    Implaments                              ///
         ///                         to                                 ///
        ///            Interpret and Parse Command Line                ///
==================================================================================================================================
*/

fn evaluate_condition(context: &ExecutionContext, condition: &str) -> bool {
    // Implement the evaluation of conditions based on the G-code specification
    // and the context (variable values) available in the `context` parameter
    // Return true if the condition is true, otherwise false
    unimplemented!()
}

fn execute_commands(context: &mut ExecutionContext, commands: &str) {
    // Implement the execution of G-code commands based on the G-code specification
    // and update the `context` (variable values) accordingly
    unimplemented!()
}

impl ModalState {
    fn new() -> Self {
        // Initialize the default modal state values
        ModalState {}
    }
}


impl Variables {
    fn new() -> Self {
        // Initialize the variables map
        Variables {}
    }
}

impl ExecutionContext {
    fn new() -> Self {
        // Implement the initialization of the execution context
        // with default values for variables
        unimplemented!()
    }

    fn get_variable_value(&self, variable: &str) -> Option<f64> {
        // Implement the method to retrieve the value of a variable
        // from the execution context
        unimplemented!()
    }

    fn set_variable_value(&mut self, variable: &str, value: f64) {
        // Implement the method to update the value of a variable
        // in the execution context
        unimplemented!()
    }

    fn execute_gcode(&mut self, gcode: &str) {
        let lines: Vec<&str> = gcode.split('\n').collect();
        for line in lines {
            let trimmed_line = line.trim();
            if trimmed_line.is_empty() {
                continue;
            }
            let command = self.parse_command(trimmed_line);
            self.interpret_command(&command);
        }
    }

    fn parse_command(&self, line: &str) -> Command {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        if tokens.is_empty() {
            return Command::Default;
        }
        match tokens[0] {
            "G" => Command::GCode(line.to_string()),
            "M" => Command::MCode(line.to_string()),
            "T" => Command::TCode(line.to_string()),
            "F" => Command::FCode(line.to_string()),
            "S" => Command::SCode(line.to_string()),
            "IF" => Command::IfStatement(line.to_string()),
            "WHILE" => Command::WhileLoop(line.to_string()),
            _ => Command::Default,
        }
    }

    fn interpret_command(&mut self, command: &Command) {
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

                if re_g0.is_match(g) {
                    // Handle G0 or G00 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G0 or G00 command: {:?}", coordinates);
                } else if re_g1.is_match(g) {
                    // Handle G01 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G1 or G01 command: {:?}", coordinates);
                } else if re_g2.is_match(g) {
                    // Handle G2 or G02 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G2 or G02 command: {:?}", coordinates);
                } else if re_g3.is_match(g) {
                    // Handle G3 or G03 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G3 or G03 command: {:?}", coordinates);
                } else if re_g4.is_match(g) {
                    // Handle G4 or G04 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G4 or G04 command: {:?}", coordinates);
                } else if re_g5.is_match(g) {
                    // Handle G5 or G05 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G5 or G05 command: {:?}", coordinates);
                } else if re_g6.is_match(g) {
                    // Handle G6 or G06 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G6 or G06 command: {:?}", coordinates);
                } else if re_g7.is_match(g) {
                    // Handle G7 or G07 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G7 or G07 command: {:?}", coordinates);
                } else if re_g8.is_match(g) {
                    // Handle G8 or G08 command
                    let coordinates: Vec<&str> = g.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting G8 or G08 command: {:?}", coordinates);
                } else if re_g9.is_match(g) {
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

                if re_m0.is_match(m) {
                    // Handle M0 or M00 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M0 or M00 command: {:?}", coordinates);
                } else if re_m1.is_match(m) {
                    // Handle M1 or M01 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M1 or M01 command: {:?}", coordinates);
                } else if re_m2.is_match(m) {
                    // Handle M2 or M02 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M2 or M02 command: {:?}", coordinates);
                } else if re_m3.is_match(m) {
                    // Handle M3 or M03 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M3 or M03 command: {:?}", coordinates);
                } else if re_m4.is_match(m) {
                    // Handle M4 or M04 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M4 or M04 command: {:?}", coordinates);
                } else if re_m5.is_match(m) {
                    // Handle M5 or M05 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M5 or M05 command: {:?}", coordinates);
                } else if re_m6.is_match(m) {
                    // Handle M6 or M06 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M6 or M06 command: {:?}", coordinates);
                } else if re_m7.is_match(m) {
                    // Handle M7 or M07 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M7 or M07 command: {:?}", coordinates);
                } else if re_m8.is_match(m) {
                    // Handle M8 or M08 command
                    let coordinates: Vec<&str> = m.split_whitespace().skip(1).collect();
                    // Process the coordinates...
                    println!("Interpreting M8 or M08 command: {:?}", coordinates);
                } else if re_m9.is_match(m) {
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
            Command::Default => {
                // Handle default command
                println!("Interpreting default command");
            }
        }
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
    let mut context = ExecutionContext::new();

    let gcode = r#"N000 O0001 (This is a comment on code)
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
        N030 #1=ADP..."#;

    let re_goto = Regex::new(r"GOTO N(\d+)").unwrap();
    let re_if_else = Regex::new(r"(IF|ELSE|ELSE IF) ([^[]+)\[(.+)\]").unwrap();
    let re_while_end = Regex::new(r"(WHILE|END)(\d+)").unwrap();
    let lines: Vec<&str> = gcode.split('\n').collect();

    let mut line_num = 0;
    let mut nested_level = 0;
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
            let commands = &            caps[3];

            match statement_type {
                "IF" => {
                    // Evaluate the condition and execute the corresponding commands
                    let condition_result = evaluate_condition(&context, condition);
                    if condition_result {
                        execute_commands(&mut context, commands);
                        line_num += 1;
                        continue;
                    }
                }
                "ELSE IF" => {
                    // Evaluate the condition and execute the corresponding commands if the previous conditions were false
                    let condition_result = evaluate_condition(&context, condition);
                    if condition_result && nested_level == 0 {
                        execute_commands(&mut context, commands);
                        line_num += 1;
                        continue;
                    }
                }
                "ELSE" => {
                    // Execute the corresponding commands if the previous conditions were false
                    if nested_level == 0 {
                        execute_commands(&mut context, commands);
                        line_num += 1;
                        continue;
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
                    let condition_result = evaluate_condition(&context, condition);
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
}

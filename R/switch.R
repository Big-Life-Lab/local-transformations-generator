library(magrittr)

source('R/token_to_pmml.R')

#' Converts a switch statement into its corresponding PMML
#' 
#' @param expr A data.frame with the expr node that holds the switch statement
#' @param tokens A data.frame containing the master list of tokens
#' @param scope_variables Pass through variable
#' @param get_pmml_str_for_expr Pass through variable
#' @param get_pmml_str_for_token Pass through variable
convert_switch_statement_to_pmml <- function(
    expr, 
    tokens, 
    scope_variables, 
    get_pmml_str_for_expr,
    get_pmml_str_for_token
) {
    child_expr_nodes <- tokens %>%
        get_expr_tokens() %>%
        get_child_tokens_for_parent(expr, .)
   
    # Check if any of the expressions within the switch statement contain 
    # multiple expressions which are currently not supported.
    # Throw an error if found..
    setter_exprs <- child_expr_nodes[-c(1,2), ]
    for(token_index in seq_len(nrow(setter_exprs))) {
        current_child_tokens <- get_child_tokens_for_parent(
            setter_exprs[token_index, ],
            tokens
        )
        if(current_child_tokens[1, ]$text == '{' &
           tail(current_child_tokens, n = 1)$text == '}') {
            # Recreate the switch statement and the offending case code 
            # to display the user as an error message
            # Recreate the actual variable name from its mutated name
            tokens$text <- gsub(
               "\\_Mutated_[0-9]{1,}",
               "",
               tokens$text
            )
            print("Multiple expression line found in the following switch statement")
            print(tokens_to_str(tokens))
            print("The multi-line expression found is")
            print(tokens_to_str(get_descendants_of_token(
                setter_exprs[token_index, ],
                tokens
            )))
            stop('Multiple expressions in switch statement')
        }
    }
    
    # Get the name of the variable that the switch statement is switching over
    switch_variable_expr <- child_expr_nodes[2, ]
    switch_variable_name <- get_child_tokens_for_parent(
        switch_variable_expr, tokens
    )$text

    # Create a list of data.frames that has the top-level nodes that make up a 
    # each case in the switch statement.
    # The initial for loop adds the nodes upto the second to last expression.
    # For the last expression we need to check if it is a default value 
    # expression or another equality check, adding the right node in either 
    # case
    cases_nodes <- list()
    child_comma_tokens <- tokens %>%
        get_child_tokens_for_parent(expr, .) %>%
        .[.$text == ',', ]
    for(token_index in seq_len(nrow(child_comma_tokens) - 1)) {
        current_equality_triplet <- list()
        cases_nodes <-  append_to_list(
            cases_nodes,
            get_in_between_nodes(
                get_child_tokens_for_parent(expr, tokens),
                child_comma_tokens[token_index, ],
                child_comma_tokens[token_index + 1, ]
            )
        )
    } 
    closing_bracket_node <- tokens %>%
        get_child_tokens_for_parent(expr, .) %>%
        .[.$text == ')', ]
    nodes_after_last_comma <- get_in_between_nodes(
        get_child_tokens_for_parent(expr, tokens),
        tail(child_comma_tokens, n = 1), 
        closing_bracket_node
    )
    if('EQ_SUB' %in% nodes_after_last_comma$token) {
        cases_nodes <- append_to_list(
            cases_nodes,
            nodes_after_last_comma
        )
    }
    else {
        cases_nodes <- append_to_list(
            cases_nodes,
            child_expr_nodes[nrow(child_expr_nodes), ]
        )
    }
    
    # Create the PMML string that contains the switch statement
    switch_pmml <- ''
    # Reverse the list of case nodes since the last expression is the else 
    # for the if in the previous cases node. This means we need to build the 
    # PMML from the end to the beginning.
    reversed_cases_nodes <- rev(cases_nodes)
    for(cases_nodes_index in seq_len(length(reversed_cases_nodes))) {
        case_nodes <- reversed_cases_nodes[[cases_nodes_index]]
        # A three node means this is a case of the type '1' = 'a'
        if(nrow(case_nodes) == 3) {
            # If the first list of nodes is a triplet it means that there's
            # no default value so we need to make the else branch return a
            # a missing value
            else_pmml <- ifelse(
                cases_nodes_index == 1,
                '<Constant dataType="NA">NA</Constant>',
                switch_pmml
            )
            condition_pmml <- glue::glue('
                <Apply function="equal">
                    <DataField name="{switch_variable_name}"/>
                        {get_pmml_string_for_constant(case_nodes[1, ])} 
                </Apply>
                {get_pmml_str_for_expr(
                    case_nodes[3, ], 
                    tokens, 
                    scope_variables
                )}                
            ')  
            switch_pmml <- glue::glue(
                '<Apply function="if">
                    {condition_pmml}
                    {else_pmml}
                </Apply>'
            )
        }
        # Otherwise its a default case expression
        else {
            switch_pmml <- glue::glue(
                switch_pmml, 
                get_pmml_str_for_expr(
                    case_nodes,
                    tokens,
                    scope_variables
                )
            )
        }
    }
    return(switch_pmml) 
}

append_to_list <- function(x, new_item) {
    x[[length(x) + 1]] <- new_item
    return(x)
}

#' Returns the nodes between a start and end node. 
#' Determining the in between nodes is done by looking at the order within a 
#' data frames of nodes, not by looking at their IDs.
#'
#' @param tokens data.frame of nodes Used to determine the order
#' @param start data.frame containing the start node
#' @param end data.frame containing the end node
#' @return A data.frame containing the in between nodes
get_in_between_nodes <- function(tokens, start, end) {
    in_between_nodes <- tokens 
    in_between_nodes <- in_between_nodes[0, ]
    start_extracting <- FALSE
    for(token_index in 1:nrow(tokens)) {
        if(tokens[token_index, ]$id == end$id) {
            break
        }

        if(start_extracting) {
            in_between_nodes <- rbind(
                in_between_nodes, tokens[token_index, ]
            )        
        }

        if(tokens[token_index, ]$id == start$id) {
            start_extracting <- TRUE
        }
    }
    return(in_between_nodes)
}

#' Converts a data.frame of nodes into their corresponding code
#'
#' @param parse_data The data.frame of nodes to convert
#' @param A string containing the converted code
tokens_to_str <- function(parse_data) {
    # exclude empty tokens
    parse_data <- parse_data[nzchar(parse_data$text), ]
    # NOT recoverable: lines ending with white spaces
    res <- by(data = parse_data,
              # By doing this we can process all the tokens for a single line 
              # together allowing us to display them together.
              INDICES = list(parse_data$line1), 
              FUN = function(args) {
                n_tokens <- length(args$text)
                
              # Get the amount of white space to add before each token 
              n_ws <- args$col1 - c(0, args$col2[-n_tokens]) - 1
                
                # create needed whitespace
                ws <- strrep(" ", n_ws)
                
                # add whitespaces to the tokens
                paste(ws, args$text, collapse = "", sep = "")
              })
    return(res)
}

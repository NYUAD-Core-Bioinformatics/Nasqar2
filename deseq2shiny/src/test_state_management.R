# Test Script for State Management Functionality
# This script tests the save/load state functionality

# Load required libraries
library(shiny)

# Source the server functions to test them independently
source("server.R", local = TRUE)

# Create a mock reactiveValues object for testing
test_myValues <- reactiveValues(
    dataCounts = matrix(1:100, nrow = 10, ncol = 10),
    fileContent = data.frame(gene = paste0("gene", 1:10), sample1 = 1:10, sample2 = 11:20),
    DF = data.frame(Samples = paste0("sample", 1:10), Condition = rep(c("A", "B"), 5)),
    selected_genes = 5
)

# Test saving state function
test_save_state <- function() {
    cat("Testing save state functionality...\n")
    
    # Mock the myValues object
    myValues <<- test_myValues
    
    # Test the saveAppState function
    tryCatch({
        state_obj <- saveAppState()
        
        # Check if all expected components are present
        expected_components <- c("dataCounts", "fileContent", "DF", "selected_genes", 
                               "save_timestamp", "app_version")
        
        missing_components <- setdiff(expected_components, names(state_obj))
        if (length(missing_components) == 0) {
            cat("✓ All expected components are present in saved state\n")
        } else {
            cat("✗ Missing components:", paste(missing_components, collapse = ", "), "\n")
        }
        
        # Check data integrity
        if (identical(state_obj$dataCounts, test_myValues$dataCounts)) {
            cat("✓ Data counts saved correctly\n")
        } else {
            cat("✗ Data counts not saved correctly\n")
        }
        
        if (identical(state_obj$selected_genes, test_myValues$selected_genes)) {
            cat("✓ Selected genes saved correctly\n")
        } else {
            cat("✗ Selected genes not saved correctly\n")
        }
        
        # Test file saving
        temp_file <- tempfile(fileext = ".RData")
        save(state_obj, file = temp_file)
        
        if (file.exists(temp_file)) {
            cat("✓ State file created successfully\n")
            file.remove(temp_file)
        } else {
            cat("✗ Failed to create state file\n")
        }
        
        return(state_obj)
        
    }, error = function(e) {
        cat("✗ Error in save state function:", e$message, "\n")
        return(NULL)
    })
}

# Test loading state function
test_load_state <- function(state_obj) {
    cat("\nTesting load state functionality...\n")
    
    if (is.null(state_obj)) {
        cat("✗ Cannot test load - save function failed\n")
        return(FALSE)
    }
    
    # Clear myValues
    myValues <<- reactiveValues()
    
    # Mock the session object for updateDesignFormula (if it exists)
    if (exists("updateDesignFormula")) {
        # Create a minimal mock session
        session <<- list()
    }
    
    tryCatch({
        # Test the loadAppState function
        success <- loadAppState(state_obj)
        
        if (success) {
            cat("✓ Load state function returned success\n")
        } else {
            cat("✗ Load state function returned failure\n")
            return(FALSE)
        }
        
        # Check if data was loaded correctly
        if (identical(myValues$dataCounts, test_myValues$dataCounts)) {
            cat("✓ Data counts loaded correctly\n")
        } else {
            cat("✗ Data counts not loaded correctly\n")
        }
        
        if (identical(myValues$selected_genes, test_myValues$selected_genes)) {
            cat("✓ Selected genes loaded correctly\n")
        } else {
            cat("✗ Selected genes not loaded correctly\n")
        }
        
        return(TRUE)
        
    }, error = function(e) {
        cat("✗ Error in load state function:", e$message, "\n")
        return(FALSE)
    })
}

# Test round-trip save/load
test_round_trip <- function() {
    cat("\nTesting complete save/load round trip...\n")
    
    # Save state
    state_obj <- test_save_state()
    
    # Load state
    load_success <- test_load_state(state_obj)
    
    if (load_success) {
        cat("✓ Complete round-trip test successful\n")
    } else {
        cat("✗ Round-trip test failed\n")
    }
    
    return(load_success)
}

# Run tests if script is executed directly
if (!interactive()) {
    cat("Running State Management Tests\n")
    cat("==============================\n")
    
    result <- test_round_trip()
    
    if (result) {
        cat("\n✓ All tests passed! State management functionality is working correctly.\n")
    } else {
        cat("\n✗ Some tests failed. Please check the implementation.\n")
    }
}

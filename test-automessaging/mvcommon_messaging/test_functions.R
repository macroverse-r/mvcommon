# Demo Functions
# Claude is not allowed to change anything in this document
A <- function() {
  mv_title("Starting A")
  mv_text("Processing in A...")
  B()
  mv_title("Continuing A")
  B2()
  B2()
  mv_success("Finished A")
}

B <- function() {
  mv_title("Starting B")
  mv_text("Processing in B...")
  
  for (i in 1:3) {
    mv_text(paste0("Iteration ", i))
    C()
  }
  
  mv_success("Finished B")
}

B2 <- function() {
  mv_title("Starting B2")
  mv_text("Processing in B2...")
  
  for (i in 1:2) {
    mv_text(paste0("Iteration ", i))
    D2()
  }
  
  mv_success("Finished B2")
}

C <- function() {
  mv_title("Starting C")
  mv_text("Processing in C...")
  D()
  mv_success("Finished C")
}

D <- function() {
  mv_title("Starting D")
  mv_text("Processing in D...")
  E()
  mv_success("Finished D")
}

D2 <- function() {
  mv_title("Starting D2")
  mv_text("Processing in D2...")
  E()
  mv_success("Finished D2")
}

E <- function() {
  mv_title("Starting E")
  mv_text("Processing in E...")
  mv_success("Finished E")
}

# cat("=== EXECUTING A() ===\n")
# A()

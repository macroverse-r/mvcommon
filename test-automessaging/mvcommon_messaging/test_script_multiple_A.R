source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/excl/mv_title.R")

debug <- FALSE

# Test case: Script WITHOUT titles - A() should act like console calls
cat("=== Test: Script without titles ===\n")
cat("A() calls should act like console (starting at level 1)\n\n")

# No mv_title calls before this - A() should start at level 1
A()  # Should be "1. STARTING A", "2. CONTINUING A"

cat("\n")

# Second A() call - should continue numbering
A()  # Should be "3. STARTING A", "4. CONTINUING A"

cat("\n=== Expected output ===\n")
cat("1. STARTING A\n")
cat("2. CONTINUING A\n") 
cat("3. STARTING A\n")
cat("4. CONTINUING A\n")

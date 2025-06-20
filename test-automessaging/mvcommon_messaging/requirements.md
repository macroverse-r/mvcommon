# mv_title Requirements

Level-based numbering system - HIERARCHICAL SUBORDINATION

## Key Requirements

### Expected Behavior Examples

#### Case 1: A() called in console
Console: A() calls:
- mv_title("Starting A") = "1. STARTING A" (using mv_h1)
- mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)

Console: A() calls again:
- mv_title("Starting A") = "1. STARTING A" (reset numbering as it is a new call by the user, using mv_h1)
- mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)

#### Case 2: A() called in script WITHOUT titles
**(a) Case 2a: A() called twice in SAME script execution:**
Script: A() calls (no prior mv_title calls):
- mv_title("Starting A") = "1. STARTING A" (using mv_h1)
- mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)

Script: A() calls again (same execution):
- mv_title("Starting A") = "3. STARTING A" (continues numbering, using mv_h1)
- mv_title("Continuing A") = "4. CONTINUING A" (using mv_h1)

**(b) Case 2b: A() called once but script sourced twice (different executions):**
First source("script.R"):
- mv_title("Starting A") = "1. STARTING A" (using mv_h1)
- mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)

Second source("script.R"):
- mv_title("Starting A") = "1. STARTING A" (reset - new execution, using mv_h1)
- mv_title("Continuing A") = "2. CONTINUING A" (using mv_h1)

#### Case 3: A() called in script WITH titles (subordinate to current level)
**(a) Case 3a: A() subordinate to level 1 context**
Script: mv_title("Section 1") = "1. SECTION 1" (using mv_h1)
Script: A() calls:
- mv_title("Starting A") = "1.1. Starting A" (subordinate to Section 1, using mv_h2)
- mv_title("Continuing A") = "1.2. Continuing A" (subordinate to Section 1, using mv_h2)

**(b) Case 3b: A() subordinate to level 2 context**
Script: mv_title("Section 2") = "2. SECTION 2" (using mv_h1)
Script: mv_title("Subsection 2.2", level_adjust=1) = "2.2. Subsection 2.2" (subordinate to Section 1, using mv_h2)
Script: A() calls:
- mv_title("Starting A") = "2.2.1. Starting A" (subordinate to Subsection 2.2, using mv_h3)
- mv_title("Continuing A") = "2.2.2. Continuing A" (subordinate to Subsection 2.2, using mv_h3)

### Specific expected behavior in test_main.R:
(i) the first call of A() is a subordinate of "My first execution (should be level 1 - numbered 1.)":
- A() should be a level lower then without "My first execution (should be level 1 - numbered 1.)" and therefore should start at level 2
- so the first "Starting A" should be 1.1 (level 2) and "Continuing A" should be 1.2 (level 2)

(ii) the second call of A() is a subordinate of "Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.)":
- Because "Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.)" is already a level 2, so the second call of A() should start level 3.
- so, the second "Starting A" should be 2.2.1 (level 3) and "Continuing A" should be 2.2.2 (level 3)

## Level mapping clarification:
- level 1 = using mv_h1 (UPPERCASE + numbered). E.g.: "1. STARTING THE EXECUTION"
- level 2 = using mv_h2 (title case + numbered). E.g.: "1.1. Start of a loop"
- level 3 = using mv_h3 (title case + numbered). E.g., "1.1.1 Math calculation"
- level 4 = using mv_h4 (title case, no numbering). E.g., "→ Starting somehting"
- level 5+ = nothing printed (no numbering/no output).

## Notes:
- Claude cannot modify test files, like test_main.R. Run them to test the code.
- Claude cannot create new files (including test files), except output log in /tmp/test_mv_title/
- UPPERCASE indicates the use of mv_h1. mv_h2, mv_h3 and mv_h4 do not use UPPERCASE transformation.

## WARNING:
DO NOT USE TIME-BASED RESET DETECTION APPROACHES
Time-based resets are unreliable and error-prone. They have been tested and proven to fail in various scenarios. Use call stack analysis instead.
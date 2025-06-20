
# source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/excl/mv_title.R")

mv_title("My first execution (should be level 1 - numbered 1.)")

A()

mv_title("My first prints (should be level 1 - numbered 2.)")

mv_title("Subsection 1 of 'My first prints' (should be level 2 - numbered 2.1.)", level_adjust = 1)

mv_text("Hello World")

Sys.sleep(2)

mv_title("Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.)", level_adjust = 1)

mv_text("Hello Claude")

A()



# Expected outcome:
#
# > source("test_main.R")
# ═════════ 1. MY FIRST EXECUTION (SHOULD BE LEVEL 1 - NUMBERED 1.) ═════════
# ───── 1.1. Starting A ─────
#     Processing in A...
# ─── 1.1.1. Starting B
#     Processing in B...
#     Iteration 1
# → Starting C
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 2
# → Starting C
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 3
# → Starting C
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
# ✔ Finished B
# ───── 1.2. Continuing A ─────
# ─── 1.2.1. Starting B2
#     Processing in B2...
#     Iteration 1
# → Starting D2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
# → Starting D2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# ─── 1.2.2. Starting B2
#     Processing in B2...
#     Iteration 1
# → Starting D2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
# → Starting D2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# ✔ Finished A
# ═════════ 2. MY FIRST PRINTS (SHOULD BE LEVEL 1 - NUMBERED 2.) ═════════
# ───── 2.1. Subsection 1 of 'My first prints' (should be level 2 - numbered 2.1.) ─────
#     Hello World
# ───── 2.2. Subsection 2 of 'My first prints' (should be level 2 - numbered 2.2.) ─────
#     Hello Claude
# ─── 2.2.1. Starting A
#     Processing in A...
# → Starting B
#     Processing in B...
#     Iteration 1
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 2
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 3
#     Processing in C...
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
# ✔ Finished B
# ─── 2.2.2. Continuing A
# → Starting B2
#     Processing in B2...
#     Iteration 1
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# → Starting B2
#     Processing in B2...
#     Iteration 1
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
#     Processing in D2...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# ✔ Finished A


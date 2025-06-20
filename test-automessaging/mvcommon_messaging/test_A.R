

source("/home/bpeeters/MEGA/repo/macroverse/mvcommon/excl/mv_title.R")

A()

# Expected outcome:
#
# > A()
# ═════════ 1. STARTING A ═════════
#     Processing in A...
# ───── 1.1. Starting B ─────
#     Processing in B...
#     Iteration 1
# ─── 1.1.1. Starting C
#     Processing in C...
# → Starting D
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 2
# ─── 1.1.2. Starting C
#     Processing in C...
# → Starting D
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
#     Iteration 3
# ─── 1.1.3. Starting C
#     Processing in C...
# → Starting D
#     Processing in D...
#     Processing in E...
# ✔ Finished E
# ✔ Finished D
# ✔ Finished C
# ✔ Finished B
# ═════════ 2. CONTINUING A ═════════
# ───── 2.1. Starting B2 ─────
#     Processing in B2...
#     Iteration 1
# ─── 2.1.1. Starting D2
#     Processing in D2...
# → Starting E
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
# ─── 2.1.2. Starting D2
#     Processing in D2...
# → Starting E
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# ───── 2.2. Starting B2 ─────
#     Processing in B2...
#     Iteration 1
# ─── 2.2.1. Starting D2
#     Processing in D2...
# → Starting E
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
#     Iteration 2
# ─── 2.2.2. Starting D2
#     Processing in D2...
# → Starting E
#     Processing in E...
# ✔ Finished E
# ✔ Finished D2
# ✔ Finished B2
# ✔ Finished A
#

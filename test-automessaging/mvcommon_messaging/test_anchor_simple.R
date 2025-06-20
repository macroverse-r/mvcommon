# Simple anchor test
calls <- sys.calls()
if (length(calls) > 0) {
  top_env_str <- capture.output(print(sys.frame(1)))[1]
  env_hex <- gsub(".*0x([0-9a-f]+).*", "\\1", top_env_str)
  anchor <- paste0("SCRIPT_", env_hex)
} else {
  anchor <- paste0("DIRECT_", as.numeric(Sys.time()) * 1000000)
}
cat('Anchor:', anchor, '\n')
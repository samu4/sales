
user_base <- data_frame(
  user = c("mjoram", "a"),
  password = c("pass12345", "a"), 
  password_hash = sapply(c("pass12345", "a"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("Joram mike Kondi","Samuel MUTINDA")
)
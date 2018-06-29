\dontrun{
# Get templates as r objects
template <- getTemplate("global")
head(template)
template <- getTemplate("global_cf")
head(template)
template <- getTemplate("global_cru")
head(template)
template <- getTemplate("europe")
head(template)
template <- getTemplate("europe_cf")
head(template)
template <- getTemplate("europe_cru")
head(template)


# Get templates written in a folder
getTemplate("global","/home/LPJTemplates/")
getTemplate("global_cf","/home/LPJTemplates/")
getTemplate("global_cru","/home/LPJTemplates/")
getTemplate("europe","/home/LPJTemplates/")
getTemplate("europe_cf","/home/LPJTemplates/")
getTemplate("europe_cru", "/home/LPJTemplates/")
)
}

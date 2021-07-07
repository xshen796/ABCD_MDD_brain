
library(ggsegExtra)
atlas_repo <- ggseg_atlas_repos("ggsegTracula", ignore.case = TRUE)
install_ggseg_atlas(repo = atlas_repo$repo, source = atlas_repo$source)

library(ggseg)
library(ggsegTracula)
ggseg(atlas = tracula,
       mapping=aes(fill = region))

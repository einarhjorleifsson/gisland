devtools::build_readme()
pkgdown::build_site()
system("cp -rp  docs/* /net/www/export/home/hafri/einarhj/public_html/gisland/.")
system("chmod -R a+rx /net/www/export/home/hafri/einarhj/public_html/gisland")

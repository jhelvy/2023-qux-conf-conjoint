library(quarto)
library(renderthis)

# Render slides 
parts <- c(
    'introduction',
    'designing-surveys',
    'estimating-models',
    'fielding-surveys'
)

for (part in parts) {
    to_html(
        from = file.path('parts', part, 'index.Rmd'),
        to = file.path('parts', part, 'index.html')
    )
    to_pdf(
        from = file.path('parts', part, 'index.html'), 
        to = file.path('parts', part, glue::glue('{part}.pdf'))
    )
}

# Render site
quarto::quarto_render()

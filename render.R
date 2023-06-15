# library(renderthis)

# # Render slides
# parts <- c(
#     'introduction',
#     'designing-surveys',
#     'estimating-models',
#     'fielding-surveys'
# )

# for (part in parts) {
#     to_html(
#         from = file.path('parts', part, 'index.Rmd'),
#         to = file.path('parts', part, 'index.html')
#     )
#     to_pdf(
#         from = file.path('parts', part, 'index.html'),
#         to = file.path('parts', part, glue::glue('{part}.pdf'))
#     )
# }

# Render site
quarto::quarto_render()

# Merge slide pdfs into single file

pdfs <- c(
    file.path("parts", "introduction", "introduction.pdf"),
    file.path("parts", "designing-surveys", "designing-surveys.pdf"),
    file.path("parts", "estimating-models", "estimating-models.pdf"),
    file.path("parts", "fielding-surveys", "fielding-surveys.pdf")
)
pdftools::pdf_combine(
    input = pdfs,
    output = file.path('parts', '2023-qux-conf-conjoint.pdf')
)

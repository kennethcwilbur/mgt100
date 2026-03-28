# MGT 100 Slide Decks

## Change Log

### 2026-03-22, ~5:50 PM PT

**Task:** Audit and fix mgt100-01-v2.qmd, then finalize as mgt100-01.qmd

**Changes performed:**

1. **Re-rendered HTML and PDF** from mgt100-01-v2.qmd; confirmed 44 slides, no page overflows, no broken images, no missing `target="_blank"` attributes.

2. **Moved population growth slide** (originally slide 11) to just after the cancer/alcohol risk slide (slide 6), per instructor request. Verified new order in PDF.

3. **Added linebreak before "Data:"** in the Competition slide callout.

4. **Reduced Competition slide table font size** to 0.7em using a `style="font-size: 0.7em;"` div wrapper.

5. **Updated Goya competition data** to better reflect Liaukonyte, Tuchman & Zhu (2022) findings:
   - Right-leaning counties: baseline ~87, post +56.4% (was ~52% with flat post-period)
   - Left-leaning counties: baseline ~158, post +9.1% (was 0% — incorrectly flat)
   - Both regions now show dissipation in weeks 2-3, matching the paper's finding that the effect disappeared within 3 weeks
   - Updated both `data/goya_sales.rds` and the slide table

6. **Created and then removed a visualization slide** (slide 43 with ggplot line chart of the Goya data). Image `images/goya_competition_viz.png` remains available but is not used in the deck.

7. **Renamed** mgt100-01-v2.qmd → mgt100-01.qmd (done by user); re-rendered as mgt100-01.html and mgt100-01-slides.pdf.

8. **Pushed to GitHub** (`kennethcwilbur/mgt100`): updated .qmd, .html, .rds, plus new template files (custom.css, gtag.html, mathjax-config.js), 11 new images, and the slides PDF.

**Verification:** All changes verified by re-rendering HTML → PDF → screenshot inspection. Final deck: 44 slides, 44 PDF pages.

**Audit items noted (not yet addressed):**
- 9 `<!-- SUGGESTED CALLOUT: review -->` markers remain in AI-drafted callouts (lines 47, 71, 243, 452, 466, 492, 513, 522, 536)
- 3 images at `height="600px"` in column layouts (cancer.png, goya1.png, goya2.png) — render OK but height attribute is misleading
- Several text-only bullet slides lack callout notes per CLAUDE.md Step 4 guidelines

### 2026-03-22, ~6:15 PM PT

**Task:** Push to GitHub and fix rendering

1. Pushed all root-level files, 11 new images, updated `data/goya_sales.rds`, and `mgt100-01-slides.pdf` to `kennethcwilbur/mgt100`.
2. Fixed accidental `index.html` overwrite (restored remote version).
3. Pushed updated `mgt100-01_files/` directory — the new clean-revealjs template requires updated revealjs libs, chalkboard plugin, and renamed CSS files that weren't in the repo.

### 2026-03-25, ~2:00-5:00 PM PT

**Task:** Reformat and revise mgt100-02-v2.qmd, finalize as mgt100-02.qmd, push to GitHub

**Changes performed:**

1. **Title** updated to "Segmentation, Mapping, Text, LLMs"
2. **Slide 50 (The Challenge):** Fixed LaTeX rendering — wrapped exponent expressions in `$...$` math mode; fixed typo `10,000^5` → `10,000^6` for N=6
3. **Slide 44 (MDS):** Grammar fix "PCA can projects" → "PCA can project"
4. **Slide 40 (PCA FAQ):** Changed "independent component analysis" → "correspondence analysis"
5. **Slides 45-48 (Netzer et al., demand modeling):** Rewrote callouts — added convergent validity, N-grams discussion question, demand modeling vs PCA question
6. **Slide 49 (Text data section header):** Added "Reinforcement Learning" to outline; changed "What are they doing" → "How do they work"; removed "What does it all mean?"
7. **Slide 51 (Embeddings):** Removed SUGGESTION prefix, changed "How is this similar" → "How does this compare", reduced image to 200px
8. **Slide 52 (Cool things about embeddings):** Deleted first bullet, reduced image, rewrote callout with word-math examples (Paris, dollar, Google, Triton), added Mikolov et al. and Pennington et al. source links
9. **Slides 53-54 (Concept Space, Sentences):** Moved after slide 52; removed titles; wrote new callouts explaining 13-dim → 2-dim reduction and autoregressive model question
10. **Slide 55 (Many ways to encode embeddings):** Updated source links — added Boykis (2025)
11. **Slide 56 (RLHF) — NEW:** Added RL diagram, two bullets defining RL and HF, dog-training callout with discussion question, OpenAI and Raschka source links
12. **Slide 57 (How LLMs work) — NEW:** Replaced old 7-step description with corrected version: training summary (pre-train + RLHF), 5-step inference loop (embed, attend, sample, append, repeat), reasoning callout, three Karpathy/Vaswani source links
13. **Deleted slides:** Old "LLMs: Given a prompt" 7-step slide, "What LLMs Can and Can't Do", "What happens next?"
14. **Slide 58 (Class script):** No changes
15. **Slide 60 (Competition):** Rewrote as K-Means clustering assignment with survey data; "the students" → "respondents"; added callout
16. **Slide 61 (Recap):** Rewrote bullets — new first bullet "Customer needs best predict customer behavior, not demos"; added K-Means, embeddings, and LLM bullets; fixed "aide" → "aid"; added `{.smaller}`
17. **Slide 62 (Going further):** Added Applied Causal Inference book link; removed callout
18. **Renamed** mgt100-02-v2.qmd → mgt100-02.qmd; removed v2 files
19. **Pushed to GitHub** (`kennethcwilbur/mgt100`): .qmd, .html, -slides.pdf, .pdf, R script, `mgt100-02_files/`, 3 new images (firefox_usertypes_all.png, gbk_segmentation.png, rl_diagram_transparent_bg.png)

**Verification:** All 62 slides screenshotted from PDF and visually inspected. All 35 referenced images confirmed loading (HTTP 200) on GitHub Pages. HTML slides, PDF, and R script all accessible at kennethcwilbur.github.io/mgt100/.

**Note for remaining decks:** Index page links to `mgt100-XX.pdf` but rendered PDFs use `mgt100-XX-slides.pdf`. Each deck needs a copy with both names (or update index.html). Also, slide 60 "HERE" still needs a download URL for the class survey data.

### 2026-03-27, ~9:00-10:00 AM PT

**Task:** Apply clean-revealjs template to mgt100-03.qmd (Demand Modeling, Week 3)

**Changes performed:**

1. **YAML rewrite** — replaced old `revealjs` format block with clean-revealjs template (title bolded, structured author block with HTML links, `date: today`, dynamic date-format, chalkboard, custom.css, gtag.html)
2. **6 `::: aside` → `::: {.source-url}`** conversions — Wright (1928) ×2, theory paper footnote, Pattnaik (surge pricing), shrinkflation links, simulation source code
3. **16 images resized** — all converted from `width="Xin"` to `height="Npx"`. Shared images matched to decks 1 & 2: intermission.jpg (400px centered), scroll.jpg/recap.png/takingoff.png (absolute bottom-right 100px)
4. **~15 code-block sub-bullets → proper nested lists** — converted 8-space/6-space indented text that rendered as code blocks to standard 4-space-indent sub-bullets throughout
5. **2 two-column layouts added** — footsize.png (portrait, slide 27) and surge.webp (portrait, slide 29) placed in `:::: {.columns}` layouts per the portrait-image rule
6. **Malformed link fixed** — shrinkflation Reddit link had `{target="_blank"}` inside URL parentheses
7. **Slide header comment removed** — `## <!--#Intermission-->` → bare `##`
8. **`.scrollable .smaller` preserved** on 2 MNL math slides per user instruction
9. **`{.smaller}` added to 7 slides** to fix overflows: Demand Curves: Challenges, MNL Pros, MNL Cons, 2. Fundamental issue, 10. General model interpretation, 11a. Simulation, Common solutions
10. **Shrinkflation image reduced** from 350px to 280px to fix callout overflow
11. **35 SUGGESTION-prefixed callout notes written** for all `##` content slides, drawing on existing speaker notes where available
12. **Slide elements reordered** to canonical order: title → bullets/image → callout → source-url → notes

**Verification:** Rendered HTML → PDF → screenshotted all 45 pages. All 45 slides render on exactly 45 PDF pages (down from 52 before overflow fixes). No broken images, no cut-off content, all math renders correctly, all two-column layouts display properly, all standard recurring images match deck 1/2 positioning.

**Template updates:** Added "Standard recurring images" table to CLAUDE.md documenting the 5 standard images and their exact markup. Updated two-column layout guidance to specify portrait-image rule.

**Follow-up fixes (same session):**

13. **Competition slide added** (slide 44) with SUGGESTION-prefixed assignment bullets about MNL estimation with smartphone data. `competition.png` absolute bottom-right 120px, matching decks 1 & 2
14. **Typo fixed** in Recap: "extesnible" → "extensible"
15. **Source-url updated** on MNL Pros: identified `Theory_Transfer.pdf` as Andrews, Fudenberg, Lei, Liang, and Wu (2023), "The Transfer Performance of Economic Models"

**Verification:** Re-rendered HTML → PDF. Final deck: 46 slides, 46 PDF pages. Competition slide, typo fix, and citation all verified via screenshots.

**Items for revision:**
- All 36 callouts (including Competition) are marked `SUGGESTION:` and need user review
- Competition slide assignment content is placeholder — needs actual assignment details
- Competition slide needs to be added to all subsequent decks (05, 06, 07, 09, 10, conjoint)

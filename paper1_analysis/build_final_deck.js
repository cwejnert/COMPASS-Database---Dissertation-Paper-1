// =============================================================================
// COMPASS Paper 1 — final deck.
// Diagnostics -> Methodology -> Conclusions -> Why.
//
// Every number here comes from FINAL_RESULTS.rds (window 2020-2050, three
// outcome families, cluster-robust intervals). Figures V1-V6 are built by
// V1_figs.R from that same file, so deck and figures cannot disagree.
// =============================================================================
const P = require("pptxgenjs");
const p = new P();
p.layout = "LAYOUT_WIDE";                       // 13.33 x 7.5 in

const NAVY="12263A", TEAL="1C7293", WHITE="FCFCFB";
const CMT="2F79BF", RE="C68A20", INK="1F2A36", MUTE="5B6B7B",
      PANEL="F1F5F9", LINE="D9E1E8", GREEN="1B7A4B", RED="A33A3A", GOLD="B7791F";
const F="Calibri", FH="Cambria";
let N=0;

function slide(dark){ N++; const s=p.addSlide(); s.background={color: dark?NAVY:WHITE};
  if(!dark) s.addText(String(N), {x:12.75,y:7.02,w:0.4,h:0.28,fontSize:9,color:MUTE,fontFace:F,align:"right"});
  return s; }

function head(s,kick,title,tag){
  if(kick) s.addText(kick.toUpperCase(), {x:0.62,y:0.42,w:9,h:0.26,fontSize:10.5,bold:true,
    color:(tag&&tag.c)||TEAL,fontFace:F,charSpacing:1.6});
  s.addText(title, {x:0.62,y:0.70,w:12.1,h:0.60,fontSize:23,bold:true,color:INK,fontFace:FH,shrinkText:true});
  if(tag&&tag.t) s.addText(tag.t,{x:11.0,y:0.40,w:1.7,h:0.3,fontSize:9,bold:true,color:tag.c||TEAL,
    fontFace:F,align:"right",charSpacing:1.2});
  s.addShape(p.ShapeType.rect,{x:0.62,y:1.42,w:12.1,h:0.02,fill:{color:LINE}});
}

function section(label,title,sub){
  const s=slide(true);
  s.addText(label.toUpperCase(),{x:0.9,y:2.5,w:9,h:0.3,fontSize:12,bold:true,color:"7FB3C8",
    fontFace:F,charSpacing:2.2});
  s.addText(title,{x:0.9,y:2.9,w:11,h:1.0,fontSize:40,bold:true,color:"FFFFFF",fontFace:FH});
  if(sub) s.addText(sub,{x:0.9,y:4.0,w:10.4,h:1.0,fontSize:14,color:"C7D6E0",fontFace:F,lineSpacing:22});
}

function img(kick,title,file,capA,capB,tag,ratio){
  const s=slide(); head(s,kick,title,tag);
  const H=(ratio||0.52)*11.0, y=1.68;
  s.addImage({path:file,x:1.0,y:y,w:11.3,h:Math.min(H,4.4)});
  const cy=y+Math.min(H,4.4)+0.16;
  if(capA) s.addText(capA,{x:0.62,y:cy,w:12.1,h:0.34,fontSize:11.5,bold:true,color:INK,fontFace:F});
  if(capB) s.addText(capB,{x:0.62,y:cy+0.34,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

function table(kick,title,headers,rows,colW,tag,note,fs){
  const s=slide(); head(s,kick,title,tag);
  const body=[headers.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:(fs||11)-1.5,
    fill:{color:PANEL},fontFace:F}}))];
  rows.forEach(r=>body.push(r.map((c,i)=>{
    const o={fontSize:fs||11,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){ Object.assign(o,c.o||{}); return {text:c.t,options:o}; }
    return {text:c,options:o};
  })));
  s.addTable(body,{x:0.62,y:1.62,w:12.1,colW:colW,border:{type:"solid",color:LINE,pt:0.5},
    rowH:0.28,autoPage:false});
  if(note) s.addText(note,{x:0.62,y:6.62,w:12.1,h:0.62,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

function bullets(kick,title,items,tag,note){
  const s=slide(); head(s,kick,title,tag);
  s.addText(items.map(t=>({text:t,options:{bullet:{code:"25AA"},breakLine:true}})),
    {x:0.72,y:1.72,w:11.9,h:4.7,fontSize:14,color:INK,fontFace:F,lineSpacing:26,paraSpaceAfter:8});
  if(note) s.addText(note,{x:0.62,y:6.6,w:12.1,h:0.6,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

// ============================== TITLE ========================================
{ const s=slide(true);
  s.addText("COMPASS Scenario Database · Paper 1",{x:0.9,y:2.1,w:10,h:0.34,fontSize:13,
    bold:true,color:"7FB3C8",fontFace:F,charSpacing:1.8});
  s.addText("High carbon management or high renewables?",
    {x:0.9,y:2.55,w:11.4,h:1.5,fontSize:42,bold:true,color:"FFFFFF",fontFace:FH,lineSpacing:48});
  s.addText("Wellbeing outcomes across ten world regions and two levels of ambition",
    {x:0.9,y:4.15,w:10.6,h:0.5,fontSize:16,color:"C7D6E0",fontFace:F});
  s.addShape(p.ShapeType.rect,{x:0.9,y:4.85,w:2.2,h:0.03,fill:{color:GOLD}});
  s.addText("590 scenarios · 24 models · 3 outcome families · 66 comparisons · cumulative 2020–2050",
    {x:0.9,y:5.1,w:10,h:0.34,fontSize:12,color:"9FB6C4",fontFace:F});
}

// ============================== 1. DIAGNOSTICS ===============================
section("Part one","Diagnostics","What is actually in the database, and what can it support?");

table("The sample","What the analysis rests on",
  ["","A — full database","C — SCI 2025 vetted"],
  [["Scenarios classified","590","137"],
   ["Reporting all ten regions",{t:"537  (91%)",o:{bold:true}},{t:"132  (96%)",o:{bold:true}}],
   ["Models / model families","24 / 12","15 / 7"],
   ["High-CMT / High-RE","335 / 255","74 / 63"],
   [{t:"Independent model × scenario-family clusters",o:{color:RED}},{t:"312  (design effect 1.9×)",o:{bold:true,color:RED}},"—"],
   ["Scenarios passing the mortality gate",{t:"501  (85%)",o:{bold:true}},{t:"135  (99%)",o:{bold:true}}]],
  [4.3,3.9,3.9], {t:"DIAGNOSTIC",c:TEAL},
  "Classification: within each ambition class, top tercile of cumulative carbon management (land CDR + novel CDR + fossil CCS) and of cumulative renewable capacity. High on one axis and not the other. High on both, or neither, is excluded: of the scenarios reporting BOTH axes that removes 44% at 2°C and 37% at 1.5°C — so it is a contrast between two CORNERS, not a dose–response. Median 2°C deployment: High-CMT 647k carbon management / 1.60M renewables; High-RE 100k / 3.13M.");

img("Model composition","The two arms are not balanced",
  "Q3_model_share.png",
  "REMIND-MAgPIE supplies 59% of ALL High-RE scenarios and none of the High-CMT. Only 7 of 12 families hold both arms.",
  "Twelve families, 24 model versions; the top three are 59% of the sample. This is a property of the database, not a flaw in the method — but a pooled comparison can pick up differences between MODELS and report them as differences between pathways. Every outcome is therefore checked against the within-model variance share, and every interval is bootstrapped over model × scenario-family clusters rather than over scenarios.",
  {t:"DIAGNOSTIC",c:TEAL}, 0.44);

table("Data quality","Four things the audit found, and what each cost",
  ["Issue","What was found","Effect on the result"],
  [[{t:"Split threshold sample",o:{bold:true}},"367 of 1,425 scenarios (26%) report NO renewable capacity, so the two terciles are cut on different samples. That is why the arms are 335 / 255 rather than equal.","Rebalancing to common support gives 62/62 and 224/224, agrees on 92% of cells, moves the headline 85% → 82%. Three cells change sign, all |δ| ≤ 0.06."],
   [{t:"Scenarios are not independent",o:{bold:true}},"590 scenarios sit in 312 model × scenario-family clusters — e.g. 30 REMIND-MAgPIE ENGAGE-NPi2020 variants counted as 30 draws.",{t:"Cluster bootstrap costs 13 of 98 significant cells; nothing is gained. All significance in the paper is now cluster-robust.",o:{color:RED}}],
   [{t:"Two measures per family",o:{bold:true}},"The two deprivation measures correlate ρ = 0.99, the two jobs measures ρ = 0.97. Counting five outcomes counts two results twice.","Headline is now 66 family-cells, not 110. The second measure in each family is reported as a within-family check."],
   [{t:"Per-capita denominator",o:{bold:true}},"pop_mln is a fixed base-period vector (7,625 mln), identical in every scenario.",{t:"Cannot touch any contrast — Cliff's δ on raw totals equals δ per capita to 0.000. Affects cross-REGION levels only.",o:{color:GREEN}}]],
  [2.7,5.1,4.3], {t:"DIAGNOSTIC",c:TEAL},
  "Also verified: the carbon-management axis is a clean ten-region sum (no World double-count, ratio exactly 1.000); the mortality coverage gate passes 85% of BOTH arms, so it is not selecting on pathway.", 10);

// ============================== 2. METHODOLOGY ===============================
section("Part two","Methodology","Six steps. Nothing in the paper depends on anything not listed here.");

table("Method","The whole design on one slide",
  ["","Step","Choice made"],
  [["1",{t:"Sample",o:{bold:true}},"All COMPASS / AR6 scenarios with R10 detail, split into 1.5°C (high ambition) and 2°C (medium)"],
   ["2",{t:"Two axes",o:{bold:true}},"Rank on cumulative carbon management and cumulative renewable capacity, each summed over the ten regions"],
   ["3",{t:"Classify",o:{bold:true}},"Top tercile on one axis and not the other → High-CMT (335) or High-RE (255). ONE fixed global set, applied unchanged in every region, so the question is answered on the same scenarios everywhere"],
   ["4",{t:"Outcomes",o:{bold:true}},"Three families, ALL cumulated 2020–2050 — the net-zero window. Jobs (renewables minus fossil; low-carbon minus fossil) · energy deprivation (cumulative gap; headcount) · health (PM2.5 mortality via TM5-FASST). One primary measure per family carries the headline"],
   ["5",{t:"Test",o:{bold:true}},"Cliff's delta, signed so positive ALWAYS means High-RE is better. Interval from a cluster bootstrap over 312 model × scenario-family clusters. Direction is the finding; the interval is the check"],
   ["6",{t:"Guard",o:{bold:true}},"Decompose each cell into within- and between-model variance. Below 10% within-model, the cell is comparing model inventories, not pathways — and is reported as such"]],
  [0.5,2.0,9.6], {t:"METHOD",c:TEAL},
  "Why 2020–2050: it is the window in which net zero is supposed to be reached, so it is the horizon the comparison is actually about. Each outcome's re-cut was verified to reproduce the master exactly at 2020–2100 before the window was shortened — jobs, mortality (×10 decadal rectangles) and deprivation (a true annual integral) all reproduce it, so the window is a parameter rather than a fork in the pipeline.");

img("Method","What “High-CMT” means, region by region",
  "P2a_cmt_composition.png",
  "Middle East 77% fossil CCS. Latin America 73% land-based CDR. World 43% / 40%.",
  "The carbon-management axis changes character across the map. Where fossil CCS dominates, High-CMT keeps the fossil fleet and its workforce. Where land-based CDR dominates it is afforestation, which creates no energy jobs at all. Novel CDR contributes essentially nothing at the median — so “carbon management” here is land CDR plus fossil CCS, and should be named that way in the paper.",
  {t:"METHOD",c:TEAL}, 0.48);

img("Method","And what “High-RE” means there",
  "P3_re_composition.png",
  "Solar PV is 67% of the World build, 90% in the Middle East, 84% in India+. Europe alone is wind-led.",
  "The two most solar-concentrated regions are also the two most fossil-CCS ones — those are straight solar-against-gas-with-capture contests. China+ and Latin America are the most diversified. Onshore wind only; no offshore category exists in the database.",
  {t:"METHOD",c:TEAL}, 0.48);

// ============================== 3. CONCLUSIONS ===============================
section("Part three","Conclusions","Global first, then regional.");

img("Result","High-RE wins 53 of 66 comparisons",
  "V1_scorecard.png",
  "80% of cells favour High-RE. 41 clear a cluster-robust interval; 7 go the other way significantly. Jobs is unanimous.",
  "Counting all five measures the figure is 93 of 110 (85%), but that double-counts the two families with two measures each. On one primary measure per family: jobs 22/22, energy deprivation 17/22, health 14/22. World is gold on all three families at 1.5°C and on all three at 2°C.",
  {t:"RESULT",c:GOLD}, 0.53);

table("Result — global","World, both levels of ambition",
  ["Outcome","Ambition","High-CMT","High-RE","Difference","Cliff's δ  [95% CI]"],
  [[{t:"RE − fossil jobs",o:{bold:true}},"1.5°C","6.05","13.44",{t:"+122%",o:{bold:true,color:GOLD}},{t:"+0.96  [0.91, 1.00]",o:{bold:true}}],
   [{t:"RE − fossil jobs",o:{bold:true}},"2°C","2.43","8.87",{t:"+265%",o:{bold:true,color:GOLD}},{t:"+0.92  [0.86, 0.96]",o:{bold:true}}],
   ["Low-carbon − fossil","1.5°C","7.40","14.55",{t:"+97%",o:{color:GOLD}},"+0.97  [0.92, 1.00]"],
   ["Low-carbon − fossil","2°C","3.31","9.75",{t:"+195%",o:{color:GOLD}},"+0.88  [0.81, 0.94]"],
   [{t:"Deprivation gap (GJ/cap)",o:{bold:true}},"1.5°C","13.96","10.01",{t:"−28%",o:{bold:true,color:GOLD}},{t:"+0.38  [0.12, 0.67]",o:{bold:true}}],
   [{t:"Deprivation gap (GJ/cap)",o:{bold:true}},"2°C","13.83","9.56",{t:"−31%",o:{bold:true,color:GOLD}},{t:"+0.32  [0.04, 0.53]",o:{bold:true}}],
   ["Deprivation headcount (%)","1.5°C","11.47","8.47",{t:"−26%",o:{color:GOLD}},"+0.36  [0.09, 0.65]"],
   ["Deprivation headcount (%)","2°C","11.10","7.97",{t:"−28%",o:{color:GOLD}},"+0.30  [0.04, 0.51]"],
   [{t:"PM2.5 mortality / 1,000",o:{bold:true}},"1.5°C","28.20","26.52",{t:"−5.9%",o:{bold:true,color:GOLD}},{t:"+0.47  [0.13, 0.82]",o:{bold:true}}],
   [{t:"PM2.5 mortality / 1,000",o:{bold:true}},"2°C","30.76","27.74",{t:"−9.8%",o:{bold:true,color:MUTE}},{t:"+0.33  [−0.05, 0.76]  n.s.",o:{bold:true,color:MUTE}}]],
  [3.1,1.2,1.5,1.5,1.6,3.2], {t:"RESULT",c:GOLD},
  "All values cumulative 2020–2050. Intervals are cluster-robust (2,000 bootstrap replicates over model × scenario-family clusters); under a naive Wilcoxon every one of these ten cells is significant, which is exactly why the naive test should not be used. World = the ten-region sum, restricted to scenarios reporting all ten regions.", 10.5);

img("Result — global","Nine of ten World cells clear the interval",
  "V2_world_forest.png",
  "The jobs intervals sit almost entirely above +0.85. Deprivation is solid but wider. Mortality at 2°C is the one cell that does not clear zero.",
  "Effect sizes are of very different magnitudes: Cliff's delta near 1.0 on jobs means almost every High-RE scenario beats almost every High-CMT scenario, while +0.33 on mortality means a modest majority does. Reporting the percentage gap alongside the delta keeps that distinction visible.",
  {t:"RESULT",c:GOLD}, 0.42);

table("Result — regional","Ten regions, both levels of ambition",
  ["Outcome","1.5°C","2°C","Median gap","Verdict"],
  [["RE − fossil jobs",{t:"10 / 10",o:{bold:true}},{t:"10 / 10",o:{bold:true}},"+117% / +216%",{t:"across the board",o:{color:GREEN,bold:true}}],
   ["Low-carbon − fossil",{t:"10 / 10",o:{bold:true}},{t:"10 / 10",o:{bold:true}},"+70% / +142%",{t:"across the board",o:{color:GREEN,bold:true}}],
   ["Energy deprivation gap","9 / 10","6 / 10","+26% / +21%",{t:"majority, not universal",o:{color:GOLD,bold:true}}],
   ["Deprivation headcount","9 / 10","7 / 10","+25% / +21%",{t:"majority, not universal",o:{color:GOLD,bold:true}}],
   ["PM2.5 mortality","6 / 10","6 / 10","+4% / +5%",{t:"leans High-RE, splits",o:{color:GOLD,bold:true}}]],
  [3.3,1.5,1.5,2.6,3.2], {t:"RESULT",c:GOLD},
  "Jobs is the only family with no regional exception: 44 of 44 cells across both measures, both ambition levels and all ten regions plus World, of which 41 clear a cluster-robust interval. The two weakest jobs regions are Pacific OECD (median δ +0.27, 2 of 4 cells significant) and the Reforming economies (+0.35, 3 of 4).", 12);

table("Result — vetting","Full database against SCI 2025 vetting",
  ["Outcome","A full 1.5°C","A full 2°C","C vetted 1.5°C","C vetted 2°C"],
  [["RE − fossil jobs",{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}}],
   ["Low-carbon − fossil",{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}},{t:"11 / 11",o:{bold:true,color:GOLD}}],
   ["Energy deprivation gap","10 / 11","7 / 11","9 / 11",{t:"5 / 11",o:{bold:true,color:CMT}}],
   ["Deprivation headcount","10 / 11","8 / 11","9 / 11",{t:"5 / 11",o:{bold:true,color:CMT}}],
   ["PM2.5 mortality","7 / 11","7 / 11","8 / 11",{t:"4 / 11",o:{color:MUTE}}]],
  [3.3,2.2,2.2,2.2,2.2], {t:"RESULT",c:GOLD},
  "Cells won by High-RE, out of eleven (World plus ten regions). On the three-family headline: 53/66 in the full database against 48/66 vetted. JOBS IS COMPLETELY UNAFFECTED by vetting — 22 of 22 either way. Deprivation at 2°C is where the two samples part company, and the vetted sample there rests on 137 scenarios rather than 590, so it loses power as well as changing sign in some cells.", 10.5);

img("Result — robustness","Does any of it depend on a choice we made?",
  "V3_robustness.png",
  "The share of cells favouring High-RE stays between 76% and 85% across every alternative tested.",
  "The tercile cut can be moved from the top half to the top quarter and the answer moves by three percentage points. Rebalancing the threshold sample moves it by three. Only SCI vetting moves it meaningfully, and that is a power effect concentrated in deprivation. What does NOT survive relaxation is significance, not direction.",
  {t:"RESULT",c:GOLD}, 0.47);

// ============================== 4. WHY =======================================
section("Part four","Why we see what we see","Mechanically, and whether it is plausible outside the models.");

img("Why — mechanism","Where the jobs advantage comes from: building, not demolishing",
  "V4_jobs_decomposition.png",
  "In India+, Rest of Asia and the Middle East, High-RE wins WHILE RETAINING MORE fossil workers. That is the strongest form of the result.",
  "Splitting the contrast into its two terms separates two different stories. In most regions High-RE wins because it builds more labour-intensive capacity, and in three regions it does so without any fossil job loss at all. But in the Reforming economies at 1.5°C (δ on renewable jobs only +0.07) and Pacific OECD (+0.12), the win is mostly fossil job DESTRUCTION — those cells should not be presented as employment gains.",
  {t:"WHY",c:GOLD}, 0.46);

img("Why — mechanism","A construction and manufacturing dividend",
  "P4a_jobtype_region.png",
  "Manufacturing +2.3, construction +1.5, operations and maintenance +1.1, extraction −0.2 job-years per 1,000.",
  "The pattern holds in every region without exception. Extraction is the only category where High-CMT leads — fossil CCS keeps the fuel supply chain running, which is exactly the employment a renewable substitution retires.",
  {t:"WHY",c:GOLD}, 0.42);

img("Why — mechanism","Which technologies the work sits in",
  "P4b_fuel_region.png",
  "Solar PV is the largest positive contributor in nine of eleven rows. What it displaces differs by region.",
  "Coal in the Reforming economies (−2.03, the deepest anywhere), China+ and Europe. Nuclear in North America. Gas in Latin America. Pacific OECD is the only region where High-RE builds LESS solar than High-CMT — which is why it is the weakest jobs region in the study.",
  {t:"WHY",c:GOLD}, 0.50);

img("Why — mechanism","Two numbers order the regions",
  "P2b_two_factor.png",
  "How much is left to build (ρ = +0.67), against how much has to be retired (ρ = −0.62 nuclear, −0.50 fossil).",
  "India+ has the largest remaining build on Earth and almost nothing to retire, and posts the largest jobs advantage anywhere. Pacific OECD has the smallest build and the narrowest advantage. Nothing else is needed to order the regions.",
  {t:"WHY",c:GOLD}, 0.50);

table("Why — by region","Each region in one line",
  ["Region","Jobs","High-CMT is…","Mechanism"],
  [[{t:"WORLD",o:{bold:true}},"4/4","43% fossil CCS","Median +158% on the jobs contrast. Real, but it averages over genuinely different regional stories"],
   ["India+","4/4","69% fossil CCS","Largest build on Earth, almost nothing to retire. Wins on renewable jobs (δ +0.89) with ZERO fossil job loss"],
   ["Rest of Asia","4/4","46% land CDR","Large build, tiny incumbent, and retains MORE fossil jobs — but the only region to lose every deprivation cell"],
   ["Middle East","4/4","77% fossil CCS","Purest fossil-CCS region. Also retains more fossil jobs under High-RE. Loses deprivation at 2°C (δ −0.39)"],
   ["Africa","4/4","49% land CDR","Nothing to defend. Largest jobs gap anywhere (+302%) and a clear deprivation win, but mortality goes the other way"],
   ["Latin America","4/4","73% land CDR","High-CMT is afforestation, not energy — it creates no energy jobs, so the jobs result is close to definitional here"],
   ["China+","4/4","51% fossil CCS","Second-largest incumbent workforce and still wins, because the remaining build is larger still. Wins all three families at 2°C"],
   ["North America","4/4","39% fossil CCS","Highest incumbent nuclear anywhere and still wins on jobs. The only region where mortality reverses significantly at BOTH ambition levels"],
   ["Europe","4/4","48% fossil CCS","Joint-smallest build and losses on three fronts — fossil, nuclear and bioenergy — yet the strongest all-round region: wins every family at both levels"],
   ["Reforming econ.",{t:"4/4",o:{color:GOLD}},"34% fossil CCS","The just-transition case: the jobs win is mostly fossil job loss (δ on renewable jobs +0.07 at 1.5°C), and mortality reverses"],
   ["Pacific OECD",{t:"4/4",o:{color:GOLD}},"33% fossil CCS","Smallest build anywhere and the only region building LESS solar under High-RE. Narrowest jobs margin (+29%); loses deprivation at 2°C"]],
  [2.1,0.9,2.2,6.9], {t:"WHY",c:GOLD},
  "The gradient has a mechanism: the advantage is largest where there is most left to build and least to retire, and narrowest in mature OECD systems where the build-out is nearly done and the incumbent workforce is large. Two regions — Reforming economies and Pacific OECD — win the jobs contrast for the wrong reason and should be flagged rather than counted as successes.", 9.5);

img("Why — the exceptions","Deprivation and health rarely trade off",
  "V5_tradeoff.png",
  "Eleven of 22 region-ambition cells improve on BOTH; only two are worse on both.",
  "The two outcomes correlate −0.58 pooled across scenario-regions, which looks like a hard trade-off — but that is a level effect BETWEEN regions: poorer, lower-energy regions have both a larger deprivation gap and less combustion. Within a region the correlation is only −0.07, so nothing forces a region to choose. Where High-RE loses one of the two, it is a regional mechanism, not an inevitable price.",
  {t:"WHY",c:GOLD}, 0.46);

img("Why — the exceptions","Why mortality is the weak family: the arms barely meet inside a model",
  "V6_mortality_variance.png",
  "In five regions under 10% of mortality variance is within a model family. For jobs it is 30–63% everywhere.",
  "POLES-JRC is 100% High-CMT and the REMIND family ~95% High-RE, so where the within-model share is tiny the two arms are essentially never observed inside the same model, and a pooled comparison reads the distance between two emissions inventories as a pathway effect. This is the Simpson risk behind the three significantly reversed mortality cells — North America at both levels, and the Reforming economies at 1.5°C. TM5-FASST itself validates: global 6.9 mln/yr against GBD 4.1 and GEMM 8.9.",
  {t:"WHY",c:GOLD}, 0.46);

bullets("Why — real-world plausibility","Is the mechanism credible outside the models?",
  ["Solar and wind are capital- and manufacturing-intensive, with installation labour spread across many small sites and cost almost entirely up front.",
   "Fossil generation is fuel-cost-intensive, with labour concentrated in extraction. Adding capture raises capital cost substantially but adds little labour beyond the host plant.",
   "So substituting renewable capacity for fossil-plus-capture should move employment out of extraction and into manufacturing and construction — which is the decomposition we measure, not an assumption imposed on it.",
   "It predicts the timing: the gain is front-loaded in the build-out, so the 2020–2050 window shows a larger gap than the full century.",
   "It predicts the exceptions: mature systems with little left to build and large incumbent fleets are where the advantage should narrow. Europe, Pacific OECD and the Reforming economies are exactly those, and they are exactly where it narrows.",
   "And it predicts what deprivation should do: more delivered energy per unit of mitigation effort closes the decent-living gap, which is what 17 of 22 cells show."],
  {t:"WHY",c:GOLD},
  "The honest limit: much of this is a transitional construction dividend. The operations-and-maintenance advantage (+1.1 job-years per 1,000) is the part that persists after the build-out. It is real, and much smaller than the headline.");

// ============================== CLOSE ========================================
bullets("What to take away","Five things",
  ["High-renewable pathways deliver better wellbeing outcomes in 53 of 66 comparisons (80%) — three outcome families, eleven regions, both levels of ambition. 41 clear a cluster-robust interval; 7 go the other way.",
   "The jobs result is the spine: 44 of 44 cells, both contrast measures, every region, both ambition levels, and completely unmoved by SCI vetting. It is a construction and manufacturing dividend, largest where there is most left to build — and in India+, Rest of Asia and the Middle East it arrives with NO fossil job loss at all.",
   "Energy deprivation follows in 17 of 22 cells with a median 25% gap closure, but it is the family that vetting moves: 2°C deprivation reverses in the SCI-vetted sample.",
   "Air-quality mortality leans High-RE (14 of 22) but is the weakest family, and for a diagnosable reason: in five regions under 10% of its variance is within-model, so the pooled contrast is partly reading model inventories. Report it as a near-term co-benefit with that caveat attached.",
   "Three things to state plainly: this is a contrast between two CORNERS (44% of 2°C scenarios reporting both axes fall in the excluded middle); the jobs direction is partly transmission from the ranking axis (ρ = 0.40–0.90 regionally); and two regions win the jobs contrast through fossil job destruction rather than renewable job creation."],
  {t:"SUMMARY",c:GOLD},
  "Open: Bergero / State of CDR scenarios (needs the stage-2 ixmp4 script) · the NH3 sensitivity (the two mortality files are byte-identical, so the contrast cannot yet be run).");

p.writeFile({fileName:"COMPASS_Paper1_Final.pptx"}).then(()=>console.log("slides:",N));

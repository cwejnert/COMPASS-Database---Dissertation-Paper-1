// =============================================================================
// COMPASS Paper 1 — final deck.
//
// Story -> Diagnostics -> Methodology -> Results -> The ammonia correction ->
// Mechanism -> Why, World and every region -> What we can claim.
//
// Nine regions plus World in the regional display. Pacific OECD is out of the
// regional rows (the global label does not describe it) and stays inside the
// World aggregate.
//
// Numbers come from FINAL_RESULTS_NH3.rds (mortality harmonised for ammonia)
// and W2_WITHIN.rds (the within-model test). Figures Y1-Y10 are built off the
// same files, so deck and figures agree.
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

// A region slide: profile strip on the left, mechanism and real-world on the right.
function region(name, verdict, vcolor, stats, mech, real, note){
  const s=slide(); head(s,"Region · why", name, {t:verdict,c:vcolor});
  s.addShape(p.ShapeType.rect,{x:0.62,y:1.62,w:3.55,h:4.62,fill:{color:PANEL}});
  let y=1.80;
  stats.forEach(r=>{
    s.addText(r[0],{x:0.80,y:y,w:2.0,h:0.26,fontSize:9.5,color:MUTE,fontFace:F});
    s.addText(r[1],{x:2.75,y:y,w:1.30,h:0.26,fontSize:9.5,bold:true,color:r[2]||INK,
      fontFace:F,align:"right"});
    y+=0.315;
  });
  s.addText("MECHANISM",{x:4.42,y:1.66,w:4,h:0.24,fontSize:9.5,bold:true,color:TEAL,
    fontFace:F,charSpacing:1.4});
  s.addText(mech,{x:4.42,y:1.94,w:8.3,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17});
  s.addText("IN THE REAL WORLD",{x:4.42,y:4.02,w:4,h:0.24,fontSize:9.5,bold:true,color:GOLD,
    fontFace:F,charSpacing:1.4});
  s.addText(real,{x:4.42,y:4.30,w:8.3,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17});
  if(note) s.addText(note,{x:0.62,y:6.42,w:12.1,h:0.66,fontSize:9.5,color:MUTE,fontFace:F,lineSpacing:13});
  return s;
}

// ============================== TITLE ========================================
{ const s=slide(true);
  s.addText("COMPASS Scenario Database · Paper 1",{x:0.9,y:2.1,w:10,h:0.34,fontSize:13,
    bold:true,color:"7FB3C8",fontFace:F,charSpacing:1.8});
  s.addText("High carbon management or high renewables?",
    {x:0.9,y:2.55,w:11.4,h:1.5,fontSize:42,bold:true,color:"FFFFFF",fontFace:FH,lineSpacing:48});
  s.addText("Wellbeing outcomes across nine world regions and two levels of ambition",
    {x:0.9,y:4.15,w:10.6,h:0.5,fontSize:16,color:"C7D6E0",fontFace:F});
  s.addShape(p.ShapeType.rect,{x:0.9,y:4.85,w:2.2,h:0.03,fill:{color:GOLD}});
  s.addText("590 scenarios · 24 models · 3 outcome families · 60 comparisons · cumulative 2020–2050",
    {x:0.9,y:5.1,w:10,h:0.34,fontSize:12,color:"9FB6C4",fontFace:F});
}

bullets("The answer in one slide","Three outcomes, three different verdicts",
  ["ENERGY JOBS — a real result, and the paper's finding. High-RE wins 20 of 20 cells, 19 significant, none against. Median +194% at World. It survives the hardest test available: inside a single modelling framework, 83% of model families and 20 of 22 cells still point to High-RE.",
   "ENERGY DEPRIVATION — large, consistent, and NOT separable from model composition. Pooled, High-RE closes the gap in 16 of 20 cells (median −30% at World). But only 42% of model families agree with the pooled direction, and at 2°C the within-model answer reverses in five regions. Reported with that limitation stated, not as a headline.",
   "AIR-QUALITY MORTALITY — no difference. Once ammonia is put on a common accounting basis across models, the World advantage falls from +0.47 to +0.06 and from +0.33 to +0.03. The apparent PM2.5 co-benefit of renewables-led mitigation was a difference in how modelling teams file agricultural emissions.",
   "Descriptively High-RE leads 44 of 60 comparisons, 34 significantly. But the count is the weakest way to read this: what matters is that one family is robust to model composition and the other two are not."],
  {t:"SUMMARY",c:GOLD},
  "Everything cumulated 2020–2050, the net-zero window. Significance is a cluster bootstrap over 312 model × scenario-family clusters, not a scenario-level p-value.");

bullets("The arc","How the paper gets there",
  ["THE QUESTION. Two ways to hit the same temperature target: manage carbon (capture it, remove it, keep the molecule) or displace it (build renewables). Both are in AR6. Do they deliver the same human outcomes?",
   "THE DESIGN. Classify every AR6 scenario on two global axes — cumulative CDR and renewable capacity — into High-CMT and High-RE. Hold climate ambition fixed. Compare three wellbeing outcomes across nine regions and World.",
   "THE COMPLICATION, and it shapes everything. The two arms are not populated by the same models. REMIND supplies 85% of High-RE and 1% of High-CMT. Every pooled comparison is therefore partly a pathway contrast and partly a model contrast, and the paper's job is to say which is which.",
   "THE TEST. Where a model family holds both arms, ask the question inside that model. Jobs passes. Deprivation does not. Mortality cannot be asked — only two families qualify.",
   "THE CORRECTION. Mortality had a second, identifiable defect: ammonia is 6–12% of PM2.5 mortality in most models and 0.15% in REMIND. Re-running TM5-FASST with ammonia removed for every model eliminates 86–90% of the apparent effect."],
  {t:"NARRATIVE",c:TEAL},
  "The result is a tiered claim rather than a single number, which is what the data can actually support.");

// ============================== 1. DIAGNOSTICS ===============================
section("Part one","Diagnostics","What is in the database, and what can it carry?");

table("The sample","From the database to 590 classified scenarios",
  ["Step","A — full database","C — SCI 2025 vetted","Note"],
  [["AR6 scenarios with the required variables","1,425","—","must report CDR, renewable capacity, final energy"],
   [{t:"Assigned a climate-ambition class",o:{bold:true}},"590","137","C1+C2 → 1.5°C · C3+C4 → 2°C"],
   ["  of which 1.5°C (C1 55 · C2 81)","136","—","74 High-CMT · 62 High-RE"],
   ["  of which 2°C (C3 323 · C4 131)","454","—","261 High-CMT · 193 High-RE"],
   [{t:"High-CMT (top CDR tercile, not top RE)",o:{bold:true,color:CMT}},{t:"335",o:{bold:true,color:CMT}},"74",""],
   [{t:"High-RE (top RE tercile, not top CDR)",o:{bold:true,color:RE}},{t:"255",o:{bold:true,color:RE}},"63",""],
   ["Excluded — high on both axes or neither","835","—","a BECCS-heavy scenario scores on both and is dropped"],
   ["Model families · individual models","9 · 24","—",""],
   ["Model × scenario-family clusters","312","—","design effect 1.9× on the effective sample"],
   ["Passing the mortality data gate","419","—","≥ 6 non-zero PM2.5 precursors, all 10 regions"]],
  [4.6,2.4,2.4,2.7],{t:"FUNNEL",c:TEAL},
  "The arms are 335/255 rather than equal because 367 scenarios report no renewable capacity at all, so the two terciles are computed on different samples. That asymmetry is carried through every robustness test.");

table("Model composition","Who populates which arm — the central diagnostic",
  ["Model family","High-CMT","High-RE","% of arm's own family that is High-RE","Holds both arms?"],
  [[{t:"REMIND",o:{bold:true}},{t:"3",o:{color:CMT}},{t:"217",o:{bold:true,color:RE}},"99%",{t:"barely",o:{color:RED}}],
   ["MESSAGEix","118","4","3%","barely"],
   ["IMAGE","76","1","1%",{t:"no",o:{color:RED}}],
   ["POLES-JRC","65","0","0%",{t:"no",o:{color:RED}}],
   ["AIM/CGE","25","11","31%",{t:"yes",o:{color:GREEN}}],
   ["WITCH","18","9","33%",{t:"yes",o:{color:GREEN}}],
   ["TIAM-ECN","23","0","0%",{t:"no",o:{color:RED}}],
   ["COFFEE","4","8","67%",{t:"yes",o:{color:GREEN}}],
   ["GCAM","3","5","62%",{t:"yes",o:{color:GREEN}}],
   [{t:"TOTAL",o:{bold:true}},{t:"335",o:{bold:true}},{t:"255",o:{bold:true}},"",""]],
  [3.0,1.8,1.8,3.3,2.2],{t:"THE CONFOUND",c:RED},
  "REMIND is 85% of the High-RE arm and 1% of the High-CMT arm. Only four families hold both arms with enough scenarios to compare inside the model. This single table determines how much of the rest of the deck can be believed.");

bullets("Model composition — what it means","Every pooled comparison is two things at once",
  ["THE ARMS ARE NEARLY DISJOINT BY MODEL. High-CMT is MESSAGEix, IMAGE, POLES-JRC and TIAM. High-RE is REMIND. A pooled difference between the arms is therefore a difference between pathways PLUS a difference between modelling frameworks, and nothing in the pooled estimate separates them.",
   "THIS IS NOT A FLAW IN THE ANALYSIS — IT IS A PROPERTY OF AR6. Modelling teams make characteristic technology choices. REMIND's scenario ensembles lean renewable; MESSAGEix and IMAGE ensembles lean toward carbon management. Any study classifying AR6 scenarios by technology mix inherits this.",
   "IT IS WORSE IN THE MORTALITY SUBSAMPLE. Of the 419 scenarios that pass the mortality data gate, REMIND is 95% of High-RE and exactly 0% of High-CMT. The mortality comparison is, almost literally, REMIND against everyone else.",
   "THE ONLY HONEST RESPONSE is to test each result inside the models that hold both arms, and to report which ones survive. That test is on slide 25 and it is the most consequential slide in the deck."],
  {t:"IMPLICATION",c:RED});

img("Region screening","One region cannot carry the comparison","Y5_label_coherence.png",
  "Pacific OECD is dropped from the regional results and kept in the World aggregate.",
  "Cliff's delta on each region's OWN renewable deployment. Near 1.0 means the global label describes what happens there. Pacific OECD is −0.19 and −0.07: High-RE builds no more renewables in that region than High-CMT does, so the contrast the paper claims to measure does not exist locally. Reforming economies is weak (+0.07 at 1.5°C on the jobs axis) and is retained with a flag.",
  {t:"9 OF 10",c:GOLD},0.42);

img("Poolability","Which outcomes can be compared across models at all","Y7_variance.png",
  "Mortality fails the variance screen in four of nine regions. Jobs passes everywhere.",
  "Share of total variance that sits WITHIN a model family rather than between families. Below 0.10 the cell is comparing model inventories rather than pathways. This screen is independent of the ammonia problem and of the within-model test — it says that even before those, mortality was the family least able to support a pooled comparison.",
  {t:"SCREEN",c:TEAL},0.42);

// ============================== 2. METHODOLOGY ===============================
section("Part two","Methodology","Six steps. Nothing depends on anything not listed here.");

table("Method","The whole design on one slide",
  ["Step","Choice","Why this and not something else"],
  [[{t:"1. Ambition",o:{bold:true}},"AR6 category → 1.5°C (C1+C2) and 2°C (C3+C4)","Compares pathways that reach the SAME climate outcome. Without this the comparison is just ambition."],
   [{t:"2. Axes",o:{bold:true}},"Cumulative Total CDR · installed Renewable Capacity","The two ways to close the gap. Both summed over the ten R10 regions, so the axes are GLOBAL."],
   [{t:"3. Labels",o:{bold:true}},"Top tercile on one axis, not the other","High on both = a BECCS pathway that is both at once; dropped. High on neither = neither; dropped."],
   [{t:"4. Window",o:{bold:true}},"Cumulative 2020–2050","The net-zero window, where the two strategies actually diverge. Sensitivity to 2100 is in the SI."],
   [{t:"5. Effect size",o:{bold:true}},"Cliff's delta, signed as ADVANTAGE","Rank-based, so no distributional assumption. Positive ALWAYS means High-RE is better, in every family."],
   [{t:"6. Inference",o:{bold:true}},"Cluster bootstrap, 2,000 replicates","590 scenarios sit in 312 clusters. Treating them as independent overstates precision by 1.9×."]],
  [2.0,4.0,6.1],{t:"DESIGN",c:TEAL},
  "One fixed global classification is applied unchanged in every region, so a region's result is never an artefact of relabelling scenarios inside that region. The per-region alternative is tested as a sensitivity and does not change the answer.");

bullets("Method — why three families, not five",
  "Five measures, three independent questions",
  ["The study computes five outcome measures. Two pairs are near-duplicates: the renewables-minus-fossil and low-carbon-minus-fossil job measures correlate ρ = 0.974, and the deprivation gap and headcount correlate ρ = 0.996.",
   "Counting all five would report one result twice and inflate the scorecard from 60 cells to 100 without adding information. The headline counts THREE families with one primary measure each — jobs (RE − fossil), deprivation (gap in GJ per capita), mortality (PM2.5 deaths per 1,000).",
   "The second measure in each family is reported as a within-family check. In every case it agrees in direction with the primary, which is what a correlation of 0.97+ implies.",
   "So the denominator is 3 families × 2 ambition levels × 10 rows (nine regions plus World) = 60 comparisons."],
  {t:"FRAMING",c:TEAL});

table("Method — outcome 1 of 3","Energy jobs: how the number is built",
  ["Element","Choice","Detail"],
  [[{t:"Source",o:{bold:true}},"AR6 capacity and generation by technology","Employment factors in the Rutovitz style, as used in the AR6 employment literature"],
   [{t:"Three streams",o:{bold:true}},"construction · manufacturing · operation & maintenance","Construction and manufacturing scale with GW BUILT each period; O&M scales with GW of stock in place"],
   [{t:"Fossil side",o:{bold:true}},"extraction · refining · fossil O&M","Fossil jobs are subtracted, so the measure is NET, not gross — a pathway cannot win by building alone"],
   [{t:"The measure",o:{bold:true}},{t:"renewable jobs − fossil jobs",o:{bold:true}},"Reported per 1,000 people, using a fixed base-period population so it cannot move with demography"],
   [{t:"Units",o:{bold:true}},"JOB-YEARS per 1,000 people","Decadal values × 10, rectangle integration. One job for ten years and ten jobs for one year count alike"],
   [{t:"Grouping",o:{bold:true}},"renewables = wind, solar, hydro, geothermal","Nuclear and biomass are excluded and tested separately in the SI — biomass is the substrate of BECCS, so counting it would let one pathway score on both classification axes"]],
  [2.0,3.7,6.4],{t:"JOBS",c:TEAL},
  "The measure deliberately cannot be won by fossil destruction alone: a pathway that simply shuts down coal scores zero on the renewable term. The decomposition on slide 33 separates the two.");

table("Method — outcome 2 of 3","Energy deprivation: how the number is built",
  ["Element","Choice","Detail"],
  [[{t:"Threshold",o:{bold:true}},"Kikstra et al. 2021, fig 1A","Regional decent-living final-energy thresholds in GJ/cap/yr: India+ 10, China+ 15, Africa 17, Europe 28, North America 37. Published REGIONAL values, not a rescaled global mean"],
   [{t:"Sector split",o:{bold:true}},"residential/commercial · transport · industry","DESIRE shares 6.7 / 11.8 / 3.8, normalised. Applied per sector, not just to the total"],
   [{t:"Efficiency path",o:{bold:true}},"−1.9%/yr, floored at 50%","The energy needed for decent living falls as service provisioning improves. Lands near −38% by 2040, matching DESIRE's −30 to −46%"],
   [{t:"The gap",o:{bold:true}},{t:"max(0, threshold − actual)",o:{bold:true}},"Truncated at zero: a region above its threshold gets NO credit for the surplus. So it measures deprivation, not net adequacy, and one region's surplus cannot offset another's shortfall"],
   [{t:"Second measure",o:{bold:true}},"headcount below threshold (%)","Correlates ρ = 0.996 with the gap; reported as a within-family check"]],
  [2.0,3.7,6.4],{t:"DEPRIVATION",c:TEAL},
  "The regional Gini implied by these thresholds understates observed inequality (Africa 0.33 against ~0.43), so within-region distribution is not represented — this is a regional aggregate, not a household count.");

table("Method — outcome 3 of 3","PM2.5 mortality: how the number is built",
  ["Element","Choice","Detail"],
  [[{t:"Model",o:{bold:true}},"TM5-FASST via the rfasst package","Source–receptor atmospheric chemistry; the standard reduced-form tool for scenario air quality"],
   [{t:"Precursors",o:{bold:true}},"SO₂ · NOₓ · BC · OM · NH₃ · VOC · CH₄ · CO","A scenario must report ≥ 6 with non-zero values to qualify — 419 of 590 do"],
   [{t:"Regional mapping",o:{bold:true}},"R10 → 56 FASST regions → back to R10","Disaggregated by population weight, aggregated back by summing the mapped regions"],
   [{t:"Exposure–response",o:{bold:true}},"FUSION","rfasst also returns GBD and GEMM; FUSION is used throughout and the choice is uniform across arms"],
   [{t:"The measure",o:{bold:true}},"cumulative PM2.5 deaths per 1,000 people","Decadal values × 10, same integration convention as jobs"],
   [{t:"Known defect, now corrected",o:{bold:true,color:RED}},{t:"ammonia is not reported on a common basis",o:{color:RED}},"NH₃ is 6–12% of PM2.5 mortality in IMAGE, POLES-JRC, AIM and MESSAGEix and 0.15% in REMIND. Part four re-runs the whole thing with ammonia removed for every model"]],
  [2.4,3.5,6.2],{t:"MORTALITY",c:TEAL},
  "Mortality is the only outcome requiring an external atmospheric model, and the only one whose inputs differ systematically in what the modelling teams choose to report.");

bullets("Method — inference","Why a cluster bootstrap and not a p-value",
  ["THE SCENARIOS ARE NOT INDEPENDENT. 590 scenarios come from 312 model × scenario-family clusters. A single study contributes a dozen scenarios that differ only in a policy dial; they share a model, a calibration and a baseline.",
   "TREATING THEM AS INDEPENDENT OVERSTATES PRECISION BY 1.9×. Under a naive Wilcoxon test, every one of the six World cells is significant — including the mortality cells that are, on any honest reading, zero. That is the diagnostic that motivated the change.",
   "WHAT IS DONE INSTEAD. 2,000 bootstrap replicates resampling WHOLE CLUSTERS with replacement, giving a 95% interval on Cliff's delta. A cell is significant only if that interval excludes zero.",
   "WHAT IT COSTS. Thirteen cells lose significance relative to the naive test and none gains it. Every significance claim in this deck has already paid that price."],
  {t:"INFERENCE",c:TEAL},
  "Cluster-robust intervals address dependence WITHIN a model. They do not address the fact that the two arms contain DIFFERENT models — that is what the within-model test on slide 25 is for.");

// ============================== 3. RESULTS ===================================
section("Part three","Results","Global first, then every region, then the test that matters.");

img("Result","The scorecard","Y1_scorecard.png",
  "High-RE leads 44 of 60 comparisons — but the three families behave completely differently.",
  "Jobs is unanimous. Deprivation leads in 16 of 20 with three reversals. Health is scattered around zero. Bold cells clear a cluster-robust 95% interval. Positive always means High-RE is better.",
  {t:"44 / 60",c:GOLD},0.44);

table("Result — global","World, both levels of ambition",
  ["Outcome","Ambition","High-CMT","High-RE","Change","Cliff's δ","Cluster-robust"],
  [[{t:"Energy jobs",o:{bold:true}},"1.5°C","6.05","13.44",{t:"+122%",o:{color:GREEN,bold:true}},"+0.96",{t:"significant",o:{color:GREEN}}],
   [{t:"job-years per 1,000",o:{color:MUTE}},"2°C","2.43","8.87",{t:"+265%",o:{color:GREEN,bold:true}},"+0.92",{t:"significant",o:{color:GREEN}}],
   [{t:"Energy deprivation",o:{bold:true}},"1.5°C","13.96","10.01",{t:"−28%",o:{color:GREEN,bold:true}},"+0.38",{t:"significant",o:{color:GREEN}}],
   [{t:"gap, GJ per capita",o:{color:MUTE}},"2°C","13.83","9.56",{t:"−31%",o:{color:GREEN,bold:true}},"+0.32",{t:"significant",o:{color:GREEN}}],
   [{t:"PM2.5 mortality",o:{bold:true}},"1.5°C","26.21","26.29",{t:"−0.3%",o:{color:MUTE}},"+0.06",{t:"not significant",o:{color:MUTE}}],
   [{t:"deaths per 1,000",o:{color:MUTE}},"2°C","28.01","27.26",{t:"+2.7%",o:{color:MUTE}},"+0.03",{t:"not significant",o:{color:MUTE}}]],
  [2.6,1.3,1.6,1.6,1.5,1.5,2.0],{t:"WORLD",c:GOLD},
  "Mortality figures are the ammonia-harmonised values. Before that correction they read +0.47 and +0.33, both apparently significant. Jobs medians are low in absolute terms because they are net of fossil losses and per 1,000 of TOTAL population, not of the workforce.");

img("Result — global","Where the World cells actually sit","Y2_world_forest.png",
  "Jobs is far from zero. Deprivation is clearly positive. Health sits on the line.",
  "Cliff's delta with a cluster-robust 95% interval, resampling whole model × scenario-family clusters. Gold clears zero, grey does not.",
  {t:"WORLD",c:GOLD},0.40);

table("Result — regions","Every region, every family, both ambition levels",
  ["Region","Jobs 1.5°C","Jobs 2°C","Depriv 1.5°C","Depriv 2°C","Health 1.5°C","Health 2°C","Score"],
  [
   ["WORLD",{t:"+0.96 ★",o:{color:GREEN}},{t:"+0.92 ★",o:{color:GREEN}},{t:"+0.38 ★",o:{color:GREEN}},{t:"+0.32 ★",o:{color:GREEN}},"+0.06","+0.03",{t:"6/6",o:{bold:true}}],
   ["India+",{t:"+0.97 ★",o:{color:GREEN}},{t:"+0.89 ★",o:{color:GREEN}},{t:"+0.89 ★",o:{color:GREEN}},{t:"+0.84 ★",o:{color:GREEN}},{t:"+0.48 ★",o:{color:GREEN}},"+0.06",{t:"6/6",o:{bold:true}}],
   ["Europe",{t:"+0.90 ★",o:{color:GREEN}},{t:"+0.80 ★",o:{color:GREEN}},{t:"+0.84 ★",o:{color:GREEN}},{t:"+0.64 ★",o:{color:GREEN}},"+0.22","+0.20",{t:"6/6",o:{bold:true}}],
   ["Africa",{t:"+0.86 ★",o:{color:GREEN}},{t:"+0.95 ★",o:{color:GREEN}},{t:"+0.28 ★",o:{color:GREEN}},{t:"+0.41 ★",o:{color:GREEN}},"−0.29","−0.14","4/6"],
   ["China+",{t:"+0.75 ★",o:{color:GREEN}},{t:"+0.72 ★",o:{color:GREEN}},{t:"+0.76 ★",o:{color:GREEN}},{t:"+0.39 ★",o:{color:GREEN}},{t:"−0.45 ★",o:{color:RED}},{t:"−0.50 ★",o:{color:RED}},"4/6"],
   ["Latin America",{t:"+0.91 ★",o:{color:GREEN}},{t:"+0.88 ★",o:{color:GREEN}},{t:"+0.69 ★",o:{color:GREEN}},{t:"+0.55 ★",o:{color:GREEN}},"−0.34","−0.06","4/6"],
   ["North America",{t:"+0.82 ★",o:{color:GREEN}},{t:"+0.82 ★",o:{color:GREEN}},{t:"+0.39 ★",o:{color:GREEN}},"+0.18",{t:"−1.00 ★",o:{color:RED}},{t:"−0.93 ★",o:{color:RED}},"4/6"],
   ["Middle East",{t:"+0.91 ★",o:{color:GREEN}},{t:"+0.79 ★",o:{color:GREEN}},"+0.06",{t:"−0.39 ★",o:{color:RED}},"+0.25","−0.19","4/6"],
   ["Reforming econ.","+0.28",{t:"+0.44 ★",o:{color:GREEN}},{t:"+0.28 ★",o:{color:GREEN}},"−0.01",{t:"−0.91 ★",o:{color:RED}},{t:"−0.71 ★",o:{color:RED}},"3/6"],
   ["Rest of Asia",{t:"+0.98 ★",o:{color:GREEN}},{t:"+0.88 ★",o:{color:GREEN}},{t:"−0.41 ★",o:{color:RED}},{t:"−0.51 ★",o:{color:RED}},"−0.01","+0.06","3/6"]],
  [2.1,1.5,1.4,1.55,1.45,1.55,1.45,1.15],{t:"ALL CELLS",c:GOLD},
  "★ = clears the cluster-robust 95% interval. Positive favours High-RE. Pacific OECD is excluded from these rows and retained in the World aggregate. Mortality is ammonia-harmonised throughout.",10);

img("Result — robustness","No single design choice carries the result","Y3_robustness.png",
  "Tercile cut, threshold sample, label basis, database and vetting — none flips the answer.",
  "Share of cells favouring High-RE under each alternative specification. Only SCI vetting moves the number meaningfully, and that is a power effect concentrated in deprivation: vetting cuts the sample from 590 to 137, so cells lose significance rather than changing direction.",
  {t:"ROBUST",c:GOLD},0.42);

table("Result — window sensitivity","Would running to 2100 strengthen the story? No.",
  ["","2020–2050 (primary)","2020–2100","What it means"],
  [[{t:"Cells favouring High-RE",o:{bold:true}},{t:"44 of 60",o:{bold:true}},{t:"39 of 60",o:{bold:true,color:RED}},"The longer window is worse overall"],
   ["Significantly against High-RE","9",{t:"13",o:{color:RED}},"Driven almost entirely by health"],
   [{t:"Jobs — cells",o:{bold:true}},"20/20","20/20","Unchanged, and gains one significant cell"],
   ["Jobs — median δ","0.877","0.853","Effect size edges down"],
   [{t:"Jobs — model families agreeing",o:{bold:true}},{t:"83%",o:{bold:true,color:GREEN}},{t:"62%",o:{bold:true,color:RED}},"The result becomes LESS model-robust — this is the number that matters"],
   ["Deprivation — cells","16/20","17/20","Marginally better"],
   [{t:"Deprivation — families agreeing",o:{bold:true}},"42%","42%","Unchanged. The longer window does nothing for the problem that actually limits it"],
   ["Health — cells","8/20",{t:"2/20",o:{color:RED}},"World flips from +0.06 to −0.28; Europe from +0.22 to −0.47"],
   ["Job-years composition, build share","87%","75%","O&M rises 16% → 25%, exactly as a construction-dividend mechanism predicts"]],
  [3.5,2.4,2.0,4.2],{t:"WINDOW",c:GOLD},
  "2020–2050 is the primary window because the two strategies diverge there and because post-2050 employment factors and decent-living thresholds are extrapolated far past anything observable. That reason was fixed before these numbers were computed, and it is the reason to keep — choosing a window because it scores higher is specification search and a reviewer will find it.",10);

img("Result — the decisive test","Does it survive inside a single model?","Y10_within_model.png",
  "Jobs holds. Deprivation does not. Health cannot be asked.",
  "Every pooled cell is partly a model contrast, because the arms are 85% REMIND against 1% REMIND. This asks the question inside each model family that holds both arms with at least three scenarios each. Shaded quadrants are reversals. Jobs: 83% of families agree with the pooled direction. Deprivation: 42% — barely better than a coin flip.",
  {t:"CRITICAL",c:RED},0.43);

table("Result — what each family can claim","The readiness table, honestly scored",
  ["","Jobs","Energy deprivation","Health"],
  [["Pooled cells favouring High-RE","20/20","16/20","8/20"],
   ["Significant for High-RE","19","14","1"],
   ["Significant against","0","3","6"],
   ["Holds under SCI vetting",{t:"20/20",o:{color:GREEN}},"13/20","7/20"],
   ["Holds on the depth-matched sample",{t:"20/20",o:{color:GREEN}},"17/20","9/20"],
   ["Model families agreeing with pooled",{t:"83%",o:{bold:true,color:GREEN}},{t:"42%",o:{bold:true,color:RED}},{t:"60% (2 families only)",o:{color:MUTE}}],
   ["Cells whose within-model median agrees",{t:"20/22",o:{color:GREEN}},{t:"14/22",o:{color:RED}},{t:"9/11",o:{color:MUTE}}],
   ["Passes the within-family variance screen",{t:"9/9 regions",o:{color:GREEN}},"7/9 regions",{t:"5/9 regions",o:{color:RED}}],
   [{t:"VERDICT",o:{bold:true}},{t:"a result",o:{bold:true,color:GREEN}},{t:"directional, with a stated limit",o:{bold:true,color:GOLD}},{t:"no difference",o:{bold:true,color:MUTE}}]],
  [4.4,2.4,3.3,2.0],{t:"READINESS",c:GOLD},
  "The same test that clears jobs is what convicts deprivation, so the standard cannot be dismissed as unfair. Deprivation is reported as a large and consistent pooled association that cannot be separated from model composition — which is a finding, not a failure.");

// ============================== 4. THE AMMONIA CORRECTION ====================
section("Part four","The ammonia correction",
  "How an apparent health co-benefit turned out to be an accounting difference.");

img("The defect","The two arms were never on the same basis","Y6_nh3_gap.png",
  "Ammonia is 6–12% of PM2.5 mortality in most models — and 0.15% in REMIND.",
  "Share of each scenario's PM2.5 mortality attributable to ammonia, from 37 scenarios run twice each. IMAGE 12.4%, POLES-JRC 9.4%, AIM 8.9%, MESSAGEix 6.4% against REMIND-MAgPIE 0.16% and REMIND 0.14% — a 58-fold gap. Agricultural NH₃ is roughly 85% of the global total, and in REMIND that agriculture lives in MAgPIE, so it never reaches Emissions|NH3.",
  {t:"DIAGNOSIS",c:RED},0.40);

img("The proof","It moves one arm and not the other","Y9_nh3_asymmetry.png",
  "Removing ammonia costs High-CMT 8.0% of its mortality and High-RE 0.36%. A 22-fold asymmetry.",
  "If ammonia were measurement noise it would move both arms alike. It does not, because the High-RE arm never contained ammonia to begin with — REMIND is 95% of the mortality-eligible High-RE arm and 0% of the High-CMT arm. Europe −36.0% against −0.7%, China+ −18.5% against −1.8%, North America −16.3% against −0.35%.",
  {t:"THE PROOF",c:RED},0.42);

img("The correction","What harmonising does to the result","Y8_nh3_correction.png",
  "Eighteen of twenty cells move toward High-CMT. Four change sign; none changes sign toward High-RE.",
  "TM5-FASST re-run with ammonia removed for EVERY model, on the same 590 classified scenarios, with the data-quality gate held fixed at the original run so both versions cover identical scenarios. The re-cut reproduces the published mortality cells exactly when fed the original file — max |difference| in Cliff's delta 0.000 — which is what makes the harmonised numbers trustworthy.",
  {t:"CORRECTED",c:RED},0.42);

bullets("The correction — what to conclude",
  "The co-benefit does not survive, and the correction is the conservative one",
  ["AT WORLD, 86% AND 90% OF THE EFFECT WAS AMMONIA. δ +0.472 → +0.064 at 1.5°C, and +0.332 → +0.032 at 2°C. Neither harmonised value is close to significant. Air-quality mortality does not distinguish the two pathways in the 2020–2050 window.",
   "DELETION IS THE CORRECTION MOST FAVOURABLE TO HIGH-RE. The scientifically better fix is to IMPUTE REMIND's missing agricultural ammonia rather than delete everyone's, and that would push High-RE's mortality UP — further against it, not back toward the original result. So the harmonised near-zero is an UPPER BOUND on the High-RE advantage. Both available corrections point the same way.",
   "NORTH AMERICA'S −1.00 IS NOT A FINDING. Complete separation with a zero-width bootstrap interval on 47 versus 17 scenarios is the signature of model composition, not of pathways. Reforming economies at −0.91 is in the same territory. Both are reported as unresolvable rather than as evidence that High-CMT is healthier.",
   "THIS IS A CONTRIBUTION, NOT A LOSS. Anyone using AR6 for air-quality work will hit this, and nobody has written it down. The paper reports the naive estimate, the diagnosis and the correction — which is a stronger and more useful result than the co-benefit would have been."],
  {t:"CONCLUSION",c:RED});

// ============================== 5. MECHANISM =================================
section("Part five","Mechanism","Why the jobs result looks the way it does.");

img("Mechanism","Building, not demolishing","Y4_jobs_decomposition.png",
  "The jobs advantage is a construction effect, not a fossil-destruction effect.",
  "Horizontal axis: the renewable-jobs gap between the arms. Vertical: the fossil-jobs gap. Almost every region sits far right — High-RE builds more — while the fossil term is small and mixed. In Rest of Asia, India+ and the Middle East the fossil term is POSITIVE, meaning High-RE retains MORE fossil employment and still wins. The result cannot be dismissed as an accounting artefact of shutting down coal.",
  {t:"MECHANISM",c:TEAL},0.42);

table("Mechanism","Where the job-years actually sit",
  ["Stream","Job type","High-CMT","High-RE","Gap","Share of the total gap"],
  [[{t:"BUILD",o:{bold:true}},"manufacturing","3.0","5.6",{t:"+2.6",o:{bold:true,color:GREEN}},{t:"59%",o:{bold:true}}],
   [{t:"BUILD",o:{bold:true}},"construction","3.3","4.8",{t:"+1.5",o:{color:GREEN}},"34%"],
   ["ongoing","operation & maintenance","4.7","5.3","+0.6","14%"],
   ["ongoing","extraction","0.6","0.3",{t:"−0.3",o:{color:RED}},"−7%"],
   ["ongoing","refining","0.0","0.0","0.0","0%"],
   [{t:"TOTAL",o:{bold:true}},"",{t:"11.6",o:{bold:true}},{t:"16.0",o:{bold:true}},{t:"+4.4",o:{bold:true}},"100%"]],
  [1.8,3.4,1.8,1.8,1.6,1.7],{t:"1.5°C, WORLD",c:TEAL},
  "Job-years per 1,000 people, cumulative 2020–2050. Manufacturing and construction together are 93% of the gap. This is the single most important mechanistic fact in the paper: the advantage is front-loaded and tied to the RATE of building, not to the stock in place.");

bullets("Mechanism — the two factors that order the regions",
  "How much is left to build, and how much is already there",
  ["FACTOR ONE — THE REMAINING BUILD. Correlation +0.67 between a region's renewable deployment gap and its jobs advantage. India+ has the largest deployment gap of any region (+10.31 on the renewable axis) and the largest jobs advantage (+214% median).",
   "FACTOR TWO — THE INCUMBENT FLEET. Correlation −0.50 with fossil capacity in 2020 and −0.62 with nuclear. The bigger the existing energy workforce, the more a renewable build has to overcome before it shows up as a NET gain. Reforming economies holds 2.50 TW of fossil capacity, the most of any region, and has the weakest jobs advantage (δ +0.28 at 1.5°C).",
   "WHERE BOTH HOLD, THE ADVANTAGE IS LARGEST. India+, Rest of Asia and Africa: everything to build, almost nothing to retire. Median jobs advantages +214%, +312% and +326%.",
   "WHERE NEITHER HOLDS, THE LABEL ITSELF BREAKS DOWN. Pacific OECD builds no more renewables under High-RE than under High-CMT, which is why it is out of the regional display entirely."],
  {t:"ORDERING",c:TEAL});

table("Mechanism","What High-CMT actually means, region by region",
  ["Region","Land-based CDR","Novel CDR","Fossil CCS","What the strategy is locally"],
  [["Middle East","1%","22%",{t:"77%",o:{bold:true,color:CMT}},"Keep producing hydrocarbons and capture the carbon"],
   ["India+","10%","21%",{t:"69%",o:{bold:true,color:CMT}},"Coal with capture, alongside the largest renewable build anywhere"],
   ["China+","35%","14%","51%","Balanced; heavy industry with capture"],
   ["Europe","39%","13%","48%","Mixed, with the largest bioenergy retirement of any region"],
   ["Latin America",{t:"73%",o:{bold:true,color:GREEN}},"10%","17%","Land — afforestation and soil carbon, not engineered removal"],
   ["North America","36%",{t:"25%",o:{bold:true}},"39%","The most technology-forward removal portfolio"],
   ["Africa",{t:"49%",o:{color:GREEN}},"13%","39%","Land-dominated, with a very small fossil base"],
   ["Reforming econ.","44%","22%","34%","Removal layered onto the largest incumbent fossil fleet"],
   ["Rest of Asia","46%","18%","36%","Land and capture in roughly equal measure"]],
  [2.1,2.0,1.6,1.7,4.7],{t:"CMT MIX",c:CMT},
  "Share of each region's cumulative Total CDR. 'High-CMT' is not one strategy: in the Gulf it means fossil CCS at 77%, in Latin America it means land-based removal at 73%. The wellbeing consequences of those two are not the same, and this paper measures only the energy-system consequences.");

// ============================== 6. WHY — WORLD ===============================
section("Part six","The why","Mechanically and in the real world — World first, then every region.");

bullets("Why — World","The mechanism is an engineering fact, not a modelling artefact",
  ["LABOUR INTENSITY PER UNIT OF ENERGY IS HIGHER FOR BUILDING THAN FOR FUELLING. A gas turbine needs a fuel supply chain and a handful of operators. An equivalent wind or solar build needs factories, foundations, cabling and installation crews, and the work is concentrated in the years it is being built. That is why manufacturing plus construction is 93% of the World gap.",
   "THE DIVIDEND IS FRONT-LOADED, AND THAT IS A REAL LIMITATION. O&M — the part that persists after the build-out — is only 16% of the gap to 2050. A pathway that builds fast produces a labour surge, not a permanent workforce. Extending to 2100 shifts the composition exactly as the mechanism predicts: the build share falls from 87% to 75% and O&M rises from 16% to 25%, while the effect size edges down from δ 0.88 to 0.85.",
   "FOSSIL CCS EXTENDS THE FUEL SUPPLY CHAIN, WHICH IS WHY EXTRACTION IS THE ONE CATEGORY HIGH-CMT LEADS. Capture keeps the mine and the well running. It is real employment and the measure counts it — the renewable advantage survives it, but the extraction term is the one place the comparison genuinely favours carbon management.",
   "THE REAL-WORLD EVIDENCE POINTS THE SAME WAY. Observed employment per GW installed in solar PV and onshore wind exceeds that of new thermal capacity in essentially every published national accounting, and the gap is widest in the build phase. The models are reproducing something already visible in labour statistics, not inventing it."],
  {t:"WORLD",c:GOLD});

bullets("Why — World","Why deprivation moves, and why we cannot fully trust it",
  ["THE MECHANISM IS DELIVERED FINAL ENERGY, NOT ELECTRICITY. The deprivation gap is max(0, threshold − actual final energy per capita), applied per sector and truncated at zero. A pathway closes the gap by delivering more usable energy to the sectors decent living depends on.",
   "HIGH-RE DELIVERS MORE FINAL ENERGY IN SIX OF NINE REGIONS. Rest of Asia +44%, Middle East +26%, Latin America +31%, China+ +10%, Africa +7%. Carbon management carries an energy penalty — capture equipment consumes energy, and removal competes for primary supply — so the same primary input delivers less to the end user.",
   "BUT THE POOLED NUMBER CANNOT BE SEPARATED FROM MODEL COMPOSITION. REMIND reports systematically different regional deprivation levels from MESSAGEix and IMAGE, and REMIND is 85% of one arm. At 2°C the within-model direction reverses in World, Africa, Europe, Latin America and North America — while in Rest of Asia and the Middle East it reverses the OTHER way, favouring High-RE where the pooled data says otherwise.",
   "SO THE CLAIM IS: high-renewable pathways are associated with substantially lower energy deprivation in this database, and the association is large and consistent across specifications, but it cannot be attributed to the pathway rather than to the models that populate it. That is the honest statement, and it is worth making."],
  {t:"WORLD",c:GOLD});

// ============================== 7. WHY — REGIONS =============================
section("Part seven","Region by region",
  "Nine regions, each with its own mechanism and its own real-world reading.");

region("India+ — the strongest case in the study","6 / 6",GREEN,
  [["Jobs, median gap","+214%",GREEN],
   ["Deprivation, median","−60% gap",GREEN],
   ["Health δ (harmonised)","+0.48 / +0.06"],
   ["Renewable build gap","+10.31",GREEN],
   ["Fossil jobs term","0.00"],
   ["Fossil capacity 2020","0.27 TW"],
   ["Decent-living threshold","10 GJ/cap"],
   ["Fossil CCS share of CDR","69%",CMT],
   ["Baseline PM2.5 deaths","103 / 1,000",RED]],
  "India+ has the largest remaining renewable build of any region (+10.31 on the deployment axis) and a fossil-jobs term of exactly 0.00 — the transition ADDS without subtracting, because the incumbent fleet is small and still growing rather than being retired. It is also the only region where the jobs advantage, the deprivation advantage and a positive health signal coincide. Its decent-living threshold is the lowest anywhere at 10 GJ/cap, so each unit of delivered energy closes proportionally more of the gap.",
  "India's power build is genuinely greenfield: there is no large stranded workforce to displace, and the manufacturing base for solar is being built now rather than defended. The High-CMT alternative here is 69% fossil CCS — coal with capture — which locks in both the mine and the import bill. The wellbeing case and the industrial-policy case point the same way, which is rare.",
  "Caveat: the health cell is significant only at 1.5°C and rests on 47 versus 34 scenarios. India+ also carries the highest baseline PM2.5 mortality of any region, so the ABSOLUTE stakes there are the largest even where the pathway contrast is small.");

region("Europe — the cleanest sweep, and the biggest ammonia correction","6 / 6",GREEN,
  [["Jobs, median gap","+161%",GREEN],
   ["Deprivation, median","−45% gap",GREEN],
   ["Health δ (harmonised)","+0.22 / +0.20"],
   ["Health δ BEFORE correction","+0.89 / +0.86",RED],
   ["Renewable build gap","+1.89"],
   ["Fossil jobs term","−0.80",RED],
   ["Nuclear jobs term","−0.57",RED],
   ["NH₃ effect, High-CMT","−36.0%",RED],
   ["NH₃ effect, High-RE","−0.7%"]],
  "Europe has the SMALLEST renewable build gap of any included region (+1.89) and still sweeps jobs, because the work is concentrated in manufacturing rather than raw capacity. It also has the largest simultaneous retirement of fossil (−0.92), nuclear (−0.57) and bioenergy (−0.48) employment, so the net advantage is earned against the stiffest incumbency headwind in the study.",
  "Europe is where the ammonia problem bites hardest: intensive livestock and fertiliser use make agricultural NH₃ a dominant PM2.5 precursor, so removing it cut High-CMT's modelled mortality by 36.0% and High-RE's by 0.7%. The apparent −36% health advantage was almost entirely a reporting difference. What remains (+0.21) is not significant. Europe's real health story in this window is about agriculture, not about power generation.",
  "Europe and China+ are the two regions where the global label holds most tightly on their own renewable deployment (δ 0.72–0.87), so their results carry the least classification risk.");

region("Africa — the largest jobs gap and the largest deprivation stakes","4 / 6",GREEN,
  [["Jobs, median gap","+326%",GREEN],
   ["Deprivation, median","−33% gap",GREEN],
   ["Health δ (harmonised)","−0.29 / −0.14"],
   ["Renewable build gap","+5.99",GREEN],
   ["Fossil capacity 2020","0.06 TW"],
   ["Baseline deprivation gap","71.9 GJ/cap",RED],
   ["Land-based share of CDR","49%"],
   ["Within-model depriv. 2C","−0.28",RED]],
  "Africa has essentially nothing to retire — 0.06 TW of fossil capacity, the smallest of any region — and a large build ahead of it, so the jobs advantage is the largest in the study at +326%. Its deprivation baseline is also by far the worst: a 71.9 GJ/cap shortfall against a 17 GJ/cap threshold, meaning most of the population is far below decent-living energy at the start of the window.",
  "The health result going mildly against High-RE is not surprising and not alarming: African PM2.5 is dominated by residential solid-fuel burning and mineral dust, neither of which the power-sector mix moves much in this window. The wellbeing lever in Africa is access to energy, not the cleanliness of the grid — and that is exactly what the deprivation measure captures.",
  "Caveat: at 2°C the within-model deprivation direction reverses to −0.28, so Africa is one of the cells where the pooled deprivation advantage cannot be attributed to the pathway. The jobs result is unaffected.");

region("China+ — sweeps on jobs and deprivation, loses on health","4 / 6",GOLD,
  [["Jobs, median gap","+148%",GREEN],
   ["Deprivation, median","−28% gap",GREEN],
   ["Health δ (harmonised)","−0.45 ★ / −0.50 ★",RED],
   ["Renewable build gap","+3.17"],
   ["Fossil capacity 2020","1.64 TW",RED],
   ["Fossil jobs term","−0.16"],
   ["Within-model health","−0.19",RED],
   ["NH₃ effect, High-CMT","−18.5%"]],
  "China+ carries the second-largest incumbent energy workforce anywhere (1.64 TW of fossil capacity) and still sweeps jobs and deprivation, because the remaining build is larger still. Its fossil-jobs term is small (−0.16), so unlike Europe it is not winning against a heavy retirement — it is simply adding on top of an existing system.",
  "China+ is the one region where the health result goes significantly against High-RE and SURVIVES the within-model check (MESSAGEix −0.19 agrees with the pooled −0.50). That makes it the most credible pro-High-CMT health signal in the study. The plausible mechanism is that China's PM2.5 is heavily influenced by ammonium nitrate from agriculture combined with industrial NOₓ, and the High-RE scenarios there do not reduce the agricultural component while carrying different industrial trajectories.",
  "China+ was the only region to win all three families at 2°C before the ammonia correction. It now wins two. That change is entirely attributable to the correction, not to any change in the jobs or deprivation data.");

region("North America — jobs and deprivation hold, health is unresolvable","4 / 6",GOLD,
  [["Jobs, median gap","+117%",GREEN],
   ["Deprivation, median","−21% gap",GREEN],
   ["Health δ (harmonised)","−1.00 ★ / −0.93 ★",RED],
   ["Bootstrap interval, health","[−1.00, −1.00]",RED],
   ["Nuclear capacity 2020","0.25 TW",RED],
   ["Nuclear jobs term","−0.62",RED],
   ["Novel CDR share","25%",CMT],
   ["Within-model health","+0.32"],
   ["Health arm sizes","47 vs 17",RED]],
  "North America holds the highest incumbent nuclear capacity of any region (0.25 TW) and the largest nuclear-jobs penalty (−0.62), so its jobs advantage of +117% is earned against a substantial high-skill retirement. It also has the most technology-forward carbon-management portfolio, with novel CDR at 25% of its removal mix — the highest anywhere.",
  "The health cell reads δ = −1.00 with a bootstrap interval of zero width. That is complete separation on 47 versus 17 scenarios, and it is a model-composition signature, not a pathway effect: inside MESSAGEix the sign is +0.32, favouring High-RE. Every model that contains both arms disagrees with the pooled figure. This cell is reported as unresolvable and should not be cited as evidence that carbon management is healthier in North America.",
  "This is the clearest single illustration in the deck of why the within-model test exists. A δ of −1.00 with a zero-width interval looks like the strongest result in the study and is in fact the weakest.");

region("Latin America — the land-based outlier","4 / 6",GOLD,
  [["Jobs, median gap","+141%",GREEN],
   ["Deprivation, median","−28% gap",GREEN],
   ["Health δ (harmonised)","−0.34 / −0.06"],
   ["Renewable share 2020","56%",GREEN],
   ["Renewable build gap","+5.95",GREEN],
   ["Fossil jobs term","−0.74",RED],
   ["Land-based share of CDR","73%",GREEN],
   ["Baseline deprivation gap","58.3 GJ/cap",RED],
   ["Final energy, High-RE","+31%",GREEN]],
  "Latin America already has the highest renewable share of any region at 56%, largely hydro, and still shows a +5.95 deployment gap between the arms — the additional build is wind and solar on top of an existing renewable base. Its High-RE scenarios deliver 31% more final energy per capita, which is why the deprivation advantage is solid despite a starting shortfall of 58.3 GJ/cap.",
  "Latin America is the region where High-CMT means something quite different from everywhere else: 73% of its removal is LAND-BASED — afforestation, reforestation, soil carbon — not engineered capture. That has wellbeing consequences this paper does not measure: land competition with agriculture, tenure conflict, and displacement of smallholders. The energy-system comparison favours High-RE, but the full welfare comparison in this region would need a land-use analysis the study does not contain.",
  "Flagged as a scope limitation rather than a result: for Latin America specifically, the paper's outcome set is least likely to capture what matters about the High-CMT pathway.");

region("Middle East — jobs yes, deprivation reverses","4 / 6",GOLD,
  [["Jobs, median gap","+292%",GREEN],
   ["Deprivation 1.5°C / 2°C","+0.06 / −0.39 ★",RED],
   ["Health δ (harmonised)","+0.25 / −0.19"],
   ["Renewable share 2020","14%",RED],
   ["Renewable build gap","+8.89",GREEN],
   ["Fossil jobs term","+0.20",GREEN],
   ["Fossil CCS share of CDR","77%",CMT],
   ["Final energy, High-RE","+43%",GREEN],
   ["Within-model depriv. 2C","+0.22",GREEN]],
  "The Middle East has the lowest renewable share anywhere (14%) and the second-largest deployment gap (+8.89), so the jobs advantage is the second-largest in the study at +292%. Notably its fossil-jobs term is POSITIVE (+0.20 at 2°C): High-RE scenarios there retain MORE fossil employment than High-CMT and still win on jobs, which rules out the 'wins by shutting down coal' reading entirely.",
  "For the Gulf, High-CMT is not a climate strategy layered onto the economy — it IS the economy. At 77% fossil CCS, the highest share anywhere, carbon management means continuing to produce and export hydrocarbons while capturing the emissions. High-RE means abandoning the export base. That is a political-economy difference of a completely different order from anything in Europe or India, and it is the main reason to expect the modelled pathways to diverge from what will actually be attempted.",
  "The 2°C deprivation reversal (−0.39) does NOT survive the within-model check: AIM +0.22, MESSAGEix +0.72, WITCH +0.15 all favour High-RE. This is a Simpson reversal driven by model composition, and the region should not be reported as a genuine deprivation loss.");

region("Rest of Asia — the largest deprivation reversal, and it is not real","3 / 6",GOLD,
  [["Jobs, median gap","+312%",GREEN],
   ["Deprivation δ","−0.41 ★ / −0.51 ★",RED],
   ["Health δ (harmonised)","−0.01 / +0.06"],
   ["Renewable build gap","+8.93",GREEN],
   ["Fossil jobs term","+0.09 / +0.18",GREEN],
   ["Final energy, High-RE","+44%",GREEN],
   ["Within-model depriv.","+0.43 / +0.74",GREEN],
   ["AIM / WITCH / REMIND","+0.79 / +0.74 / +0.43",GREEN]],
  "Rest of Asia has the third-largest deployment gap (+8.93), a POSITIVE fossil-jobs term at both ambition levels, and High-RE scenarios that deliver 44% MORE final energy per capita. On every mechanistic indicator it should be one of the strongest deprivation cases in the study. Instead the pooled deprivation cell reads −0.41 and −0.51, significantly AGAINST High-RE.",
  "That contradiction is the clearest Simpson's paradox in the paper. Inside AIM the delta is +0.79, inside WITCH +0.74, inside REMIND +0.43 — every substantial model family that holds both arms favours High-RE. The pooled reversal happens because MESSAGEix reports very low deprivation gaps in this region (median 0.86 GJ/cap) and populates 118 of the High-CMT scenarios, while REMIND reports much higher gaps (median 6.8–8.3) and populates 217 of the High-RE ones. The arms are comparing models, not pathways.",
  "Reported as a reversal that the within-model evidence contradicts. It is also the single best argument in the deck for why the deprivation family carries a limitation rather than a claim.");

region("Reforming economies — the weakest case, and the reason why","3 / 6",RED,
  [["Jobs δ","+0.28 / +0.44 ★"],
   ["Jobs, median gap","+127%",GREEN],
   ["Deprivation δ","+0.28 ★ / −0.01"],
   ["Health δ (harmonised)","−0.91 ★ / −0.71 ★",RED],
   ["Fossil capacity 2020","2.50 TW",RED],
   ["Fossil jobs term","−2.49",RED],
   ["Nuclear jobs term","−1.35",RED],
   ["Bioenergy jobs term","+1.91",GREEN],
   ["Label coherence 1.5°C","+0.07",RED]],
  "Reforming economies holds the largest incumbent fossil fleet of any region at 2.50 TW, and pays the largest employment penalty for it: fossil −2.49 and nuclear −1.35, the biggest retirements in the study. Its renewable-deployment gap at 1.5°C is only +0.07, meaning the global label barely distinguishes the two arms there — it is the weakest label coherence of any region still in the regional display.",
  "Russia and Central Asia are where the transition is most obviously a net industrial loss under this measure, and the models reflect that. The bioenergy term is the only strongly positive one (+1.91), which suggests the High-RE pathways there lean on biomass rather than wind and solar — the one place in the study where the renewable build is not primarily a manufacturing story.",
  "Retained with a flag rather than dropped. Its health cell (−0.91) has the same complete-separation problem as North America's and is equally unresolvable; inside MESSAGEix the sign is +0.35, favouring High-RE.");

region("Pacific OECD — excluded, and why that is the right call","EXCLUDED",RED,
  [["Renewable deployment δ","−0.19 / −0.07",RED],
   ["Jobs δ","+0.35 / +0.23"],
   ["Deprivation δ","+0.01 / −0.51",RED],
   ["Health δ (harmonised)","+0.96 / +0.93"],
   ["Final energy, High-RE","−29% / −39%",RED],
   ["Fossil jobs term","−0.21 / −0.53",RED],
   ["Status","World aggregate only"]],
  "Pacific OECD is the one region where the global classification does not describe local behaviour: High-RE scenarios build NO MORE renewables there than High-CMT scenarios do (δ −0.19 and −0.07). The contrast the paper claims to measure does not exist inside that region, so any outcome difference is measuring something other than the pathway.",
  "It is retained inside the World aggregate, because the World row is a sum over all ten regions and dropping a region there would change what 'World' means. It is removed from the regional rows only. This is a decision about display, not about data: nothing is discarded.",
  "Independently, Pacific OECD is one of two regions where the jobs decomposition shows the advantage coming through fossil destruction rather than renewable construction, and its High-RE arm delivers 29–39% LESS final energy per capita — a further sign the label is not tracking what it should there.");

// ============================== 8. CLOSE =====================================
section("Part eight","What we can claim","And what has to be said alongside it.");

bullets("The claim","Three sentences the paper can defend",
  ["ONE. In the AR6 scenario ensemble, high-renewable mitigation pathways deliver substantially more net energy employment than high-carbon-management pathways at the same climate ambition — 20 of 20 region × ambition cells, median +194% at World, and the result holds inside individual modelling frameworks (83% of model families, 20 of 22 cells). The mechanism is construction and manufacturing, which together are 93% of the gap.",
   "TWO. High-renewable pathways are also associated with substantially lower energy deprivation — 16 of 20 cells, median −30% at World, robust to every specification tested. But this association cannot be separated from model composition: only 42% of model families agree with the pooled direction, and the within-model answer reverses in both directions depending on region. It is reported as an association with that limitation stated.",
   "THREE. There is no detectable difference in air-quality mortality between the two pathway types once ammonia is placed on a common accounting basis across models. The apparent co-benefit in the uncorrected data (+0.47 at World) was 86–90% an artefact of REMIND not reporting agricultural ammonia, and the correction used is the one most favourable to renewables.",
   "The methodological finding — that AR6 emissions reporting is not comparable across modelling teams for ammonia, and that this materially changes air-quality conclusions — is a contribution in its own right and has not been documented elsewhere."],
  {t:"CLAIM",c:GOLD});

bullets("Limitations","Stated plainly, because they are load-bearing",
  ["MODEL COMPOSITION IS THE BINDING CONSTRAINT. REMIND supplies 85% of High-RE and 1% of High-CMT. This is a property of AR6, not of the analysis, and it limits what any study classifying AR6 scenarios by technology mix can claim. The paper's response is to test every result inside models and report which survive.",
   "THE JOBS DIVIDEND IS FRONT-LOADED. O&M is only 14% of the gap. The 2020–2050 result is a construction surge, not evidence of a permanently larger workforce, and the paper should not be read as claiming otherwise.",
   "DEPRIVATION IS A REGIONAL AGGREGATE. The implied within-region Gini understates observed inequality (Africa 0.33 against ~0.43), so the measure cannot speak to who inside a region is deprived.",
   "THE OUTCOME SET IS ENERGY-SYSTEM CENTRIC. It does not capture land competition, which is the dominant welfare channel for High-CMT in Latin America (73% land-based removal) and Africa (49%). For those regions the comparison is incomplete in a way that likely flatters High-CMT's measured performance.",
   "PACIFIC OECD IS EXCLUDED FROM REGIONAL DISPLAY and Reforming Economies is retained with a weak-label flag."],
  {t:"LIMITS",c:RED});

bullets("What is open","Next steps, in priority order",
  ["IMPUTE AMMONIA FOR REMIND rather than deleting it for everyone. Deletion discards real signal from the four families that do report NH₃. Imputation needs an external agricultural ammonia source and would sharpen the mortality result in both directions — it is a next-paper problem, but it is the scientifically correct fix.",
   "RE_SPEC DEFINITION SENSITIVITY. The renewables axis currently excludes nuclear and biomass, for a principled reason — biomass is the substrate of BECCS, so a BECCS-heavy scenario would score on both axes. The master supports low_carbon and with_biomass alternatives; running all three would answer the reviewer question directly. Requires two re-runs on the machine holding the interpolated database.",
   "REGIONAL EMISSIONS PROVENANCE. Scenarios reporting emissions only at World have their regional detail filled in by population weight, which carries no pathway information. A test is written; if those scenarios concentrate in one arm the regional mortality cells need re-cutting on genuine regional data.",
   "BERGERO / STATE OF CDR SCENARIOS, parked deliberately. Adding a CDR-focused ensemble moves the tercile thresholds and reclassifies scenarios unrelated to it, so it changes the comparison rather than extending it."],
  {t:"OPEN",c:TEAL});

p.writeFile({fileName:"COMPASS_Paper1_final.pptx"}).then(()=>console.log("slides:",N));

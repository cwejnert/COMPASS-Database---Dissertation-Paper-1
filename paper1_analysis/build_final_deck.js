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
// Results are reported as RAW LEVELS with a cluster-bootstrap 95% interval on
// the raw DIFFERENCE IN MEDIANS (RAW_RESULTS.rds, built by W6_raw_effects.R).
// Cliff's delta is retained only where the question is genuinely about rank
// overlap -- the within-model test. Mortality is ammonia-harmonised throughout.
// Figures Y1-Y10 are built off the same files, so deck and figures agree.
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
  // Stacked label-over-value: the raw cells carry strings like
  // "6.96 -> 19.21 · 3.88 -> 13.63", which will not fit a right-aligned column.
  s.addShape(p.ShapeType.rect,{x:0.62,y:1.62,w:3.62,h:4.62,fill:{color:PANEL}});
  const rowH = Math.min(0.50, 4.42/Math.max(stats.length,1));
  let y=1.74;
  stats.forEach(r=>{
    s.addText(r[0].toUpperCase(),{x:0.82,y:y,w:3.24,h:0.19,fontSize:7.4,color:MUTE,
      fontFace:F,charSpacing:0.7});
    s.addText(r[1],{x:0.82,y:y+0.175,w:3.24,h:0.24,fontSize:10.5,bold:true,
      color:r[2]||INK,fontFace:F,shrinkText:true});
    y+=rowH;
  });
  s.addText("MECHANISM",{x:4.50,y:1.66,w:4,h:0.24,fontSize:9.5,bold:true,color:TEAL,
    fontFace:F,charSpacing:1.4});
  s.addText(mech,{x:4.50,y:1.94,w:8.22,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17,valign:"top"});
  s.addText("IN THE REAL WORLD",{x:4.50,y:4.02,w:4,h:0.24,fontSize:9.5,bold:true,color:GOLD,
    fontFace:F,charSpacing:1.4});
  s.addText(real,{x:4.50,y:4.30,w:8.22,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17,valign:"top"});
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
  ["ENERGY JOBS — a real result, and the paper's finding. High-RE is better in 20 of 20 cells, 19 clearing the interval, none against. At World, net energy employment goes from 6.05 to 13.44 job-years per 1,000 people at 1.5°C (+122%) and 2.43 to 8.87 at 2°C (+265%). It survives the hardest test available: inside a single modelling framework, 83% of model families and 20 of 22 cells still point to High-RE.",
   "ENERGY DEPRIVATION — large, consistent, and NOT separable from model composition. High-RE closes the decent-living gap from 13.96 to 10.01 GJ per capita at 1.5°C (−28%) and 13.83 to 9.56 at 2°C (−31%), in 16 of 20 cells. But only 42% of model families agree with the pooled direction, and at 2°C the within-model answer reverses in five regions. Reported with that limitation stated, not as a headline.",
   "AIR-QUALITY MORTALITY — reported both ways, and neither supports a co-benefit. As reported, High-RE avoids 1.67 deaths per 1,000 at World — but the interval is [−0.12, +3.08] and does not clear zero. Harmonised for ammonia it is −0.08 [−0.90, +1.19]. What the mortality data DOES say is that India+ carries 44 deaths per 1,000 against Europe's 12 and North America's 10 — a 4.7-fold burden gap that no pathway choice closes.",
   "Descriptively High-RE is better in 44 of 60 comparisons, 32 significantly. But the count is the weakest way to read this: what matters is that one family is robust to model composition and the other two are not."],
  {t:"SUMMARY",c:GOLD},
  "Everything cumulated 2020–2050, the net-zero window. Every cell is reported as the two arm medians in their own units; significance is a 2,000-replicate cluster bootstrap on the raw difference between them, resampling whole model × scenario-family clusters.");

bullets("The arc","How the paper gets there",
  ["THE QUESTION. Two ways to hit the same temperature target: manage carbon (capture it, remove it, keep the molecule) or displace it (build renewables). Both are in AR6. Do they deliver the same human outcomes?",
   "THE DESIGN. Classify every AR6 scenario on two global axes — cumulative CDR and renewable capacity — into High-CMT and High-RE. Hold climate ambition fixed. Compare three wellbeing outcomes across nine regions and World.",
   "THE COMPLICATION, and it shapes everything. The two arms are not populated by the same models. REMIND supplies 85% of High-RE and 1% of High-CMT. Every pooled comparison is therefore partly a pathway contrast and partly a model contrast, and the paper's job is to say which is which.",
   "THE TEST. Where a model family holds both arms, ask the question inside that model. Jobs passes. Deprivation does not. Mortality cannot be asked — only two families qualify.",
   "THE CORRECTION. Mortality had a second, identifiable defect: ammonia is 6–12% of PM2.5 mortality in most models and 0.15% in REMIND. Re-running TM5-FASST with ammonia removed for every model takes the World gap from 1.68 deaths per 1,000 avoided to −0.08."],
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
  "High-RE is better in 44 of 60 comparisons — but the three families behave completely differently.",
  "Percentage change in the median outcome, High-CMT to High-RE. Jobs is unanimous and large. Deprivation leads in 16 of 20 with three reversals. Health sits on zero. Bold cells clear a cluster-robust 95% interval on the raw difference.",
  {t:"44 / 60",c:GOLD},0.44);

table("Result — global","World, both levels of ambition",
  ["Outcome","Ambition","High-CMT","High-RE","Raw difference","95% interval","Change"],
  [[{t:"Energy jobs",o:{bold:true}},"1.5°C","6.05","13.44",{t:"+7.39",o:{color:GREEN,bold:true}},{t:"[+6.35, +9.13]",o:{color:GREEN}},{t:"+122%",o:{color:GREEN,bold:true}}],
   [{t:"job-years per 1,000",o:{color:MUTE}},"2°C","2.43","8.87",{t:"+6.44",o:{color:GREEN,bold:true}},{t:"[+5.62, +7.20]",o:{color:GREEN}},{t:"+265%",o:{color:GREEN,bold:true}}],
   [{t:"Energy deprivation",o:{bold:true}},"1.5°C","13.96","10.01",{t:"−3.95",o:{color:GREEN,bold:true}},{t:"[+0.57, +5.87]",o:{color:GREEN}},{t:"−28%",o:{color:GREEN,bold:true}}],
   [{t:"gap, GJ per capita",o:{color:MUTE}},"2°C","13.83","9.56",{t:"−4.27",o:{color:GREEN,bold:true}},{t:"[+0.28, +5.32]",o:{color:GREEN}},{t:"−31%",o:{color:GREEN,bold:true}}],
   [{t:"PM2.5 mortality",o:{bold:true}},"1.5°C","26.21","26.29",{t:"+0.08",o:{color:MUTE}},{t:"[−0.90, +1.19]",o:{color:MUTE,bold:true}},{t:"−0.3%",o:{color:MUTE}}],
   [{t:"deaths per 1,000",o:{color:MUTE}},"2°C","28.01","27.26",{t:"−0.74",o:{color:MUTE}},{t:"[−3.11, +1.87]",o:{color:MUTE,bold:true}},{t:"+2.7%",o:{color:MUTE}}]],
  [2.6,1.2,1.4,1.4,1.9,2.2,1.4],{t:"WORLD",c:GOLD},
  "The interval is on the RAW DIFFERENCE between the arm medians, signed as advantage to High-RE, from a 2,000-replicate cluster bootstrap. Both health intervals straddle zero by more than the gap itself. Jobs medians are low in absolute terms because they are net of fossil losses and per 1,000 of TOTAL population, not of the workforce.");

img("Result — global","How big is the gap, in real units?","Y2_world_forest.png",
  "Jobs and deprivation clear zero comfortably. Health straddles it in both directions.",
  "Raw difference between the arm medians in each outcome's own units, with a cluster-robust 95% interval. Grey text gives the two medians. Health's interval is wider than its point estimate at both ambition levels, which is what a genuine null looks like.",
  {t:"WORLD",c:GOLD},0.36);

// ---- three raw tables, one per family --------------------------------------
function rawtable(kick,title,unit,rows,tag,note){
  return table(kick,title,
    ["Region","High-CMT","High-RE","Difference","High-CMT","High-RE","Difference"],
    rows,[2.5,1.6,1.6,1.75,1.6,1.6,1.75],tag,note,10.5);
}
{ const s2=slide(); head(s2,"Result — regions","Energy jobs: job-years per 1,000 people",{t:"20 / 20",c:GOLD});
  s2.addText("1.5°C HIGH AMBITION",{x:2.72,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  s2.addText("2°C MEDIUM AMBITION",{x:7.75,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  const H=["Region","High-CMT","High-RE","Difference","High-CMT","High-RE","Difference"];
  const R=[
   ["WORLD","6.05","13.44",{t:"+7.39 ●",o:{color:GREEN,bold:true}},"2.43","8.87",{t:"+6.44 ●",o:{color:GREEN,bold:true}}],
   ["Africa","2.80","10.37",{t:"+7.57 ●",o:{color:GREEN}},"1.52","7.33",{t:"+5.80 ●",o:{color:GREEN}}],
   ["China+","6.20","10.14",{t:"+3.95 ●",o:{color:GREEN}},"2.18","7.24",{t:"+5.06 ●",o:{color:GREEN}}],
   ["Europe","1.80","4.67",{t:"+2.87 ●",o:{color:GREEN}},"1.38","3.64",{t:"+2.26 ●",o:{color:GREEN}}],
   ["India+","6.96","19.21",{t:"+12.25 ●",o:{color:GREEN,bold:true}},"3.88","13.63",{t:"+9.75 ●",o:{color:GREEN,bold:true}}],
   ["Latin America","6.65","12.19",{t:"+5.54 ●",o:{color:GREEN}},"3.57","10.69",{t:"+7.12 ●",o:{color:GREEN}}],
   ["Middle East","7.90","19.31",{t:"+11.41 ●",o:{color:GREEN,bold:true}},"1.87","10.09",{t:"+8.22 ●",o:{color:GREEN}}],
   ["North America","4.88","9.28",{t:"+4.40 ●",o:{color:GREEN}},"2.94","7.15",{t:"+4.20 ●",o:{color:GREEN}}],
   ["Reforming econ.","10.77","17.18",{t:"+6.41",o:{color:MUTE}},"3.93","11.61",{t:"+7.67 ●",o:{color:GREEN}}],
   ["Rest of Asia","4.01","14.46",{t:"+10.45 ●",o:{color:GREEN,bold:true}},"2.19","10.15",{t:"+7.97 ●",o:{color:GREEN}}]];
  const body=[H.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:9,fill:{color:PANEL},fontFace:F}}))];
  R.forEach(r=>body.push(r.map(c=>{const o={fontSize:10.5,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){Object.assign(o,c.o||{});return {text:c.t,options:o};} return {text:c,options:o};})));
  s2.addTable(body,{x:0.62,y:1.78,w:12.1,colW:[2.5,1.6,1.6,1.75,1.6,1.6,1.75],
    border:{type:"solid",color:LINE,pt:0.5},rowH:0.39,autoPage:false});
  s2.addText("● clears a cluster-robust 95% interval on the raw difference. Every cell favours High-RE, and only Reforming Economies at 1.5°C fails to clear the interval — its interval is [−3.49, +15.24], which is width, not direction. The absolute numbers are net of fossil job losses and per 1,000 of total population.",
    {x:0.62,y:6.55,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
}
{ const s2=slide(); head(s2,"Result — regions","Energy deprivation: decent-living gap, GJ per capita",{t:"16 / 20",c:GOLD});
  s2.addText("1.5°C HIGH AMBITION",{x:2.72,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  s2.addText("2°C MEDIUM AMBITION",{x:7.75,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  const H=["Region","High-CMT","High-RE","Gap closed","High-CMT","High-RE","Gap closed"];
  const R=[
   ["WORLD","13.96","10.01",{t:"−3.95 ●",o:{color:GREEN,bold:true}},"13.83","9.56",{t:"−4.27 ●",o:{color:GREEN,bold:true}}],
   ["Africa","49.98","38.41",{t:"−11.57",o:{color:MUTE}},"58.62","34.03",{t:"−24.59 ●",o:{color:GREEN,bold:true}}],
   ["China+","1.13","0.75",{t:"−0.37 ●",o:{color:GREEN}},"0.76","0.59",{t:"−0.17 ●",o:{color:GREEN}}],
   ["Europe","4.60","2.01",{t:"−2.59 ●",o:{color:GREEN}},"2.92","1.92",{t:"−1.00 ●",o:{color:GREEN}}],
   ["India+","1.11","0.47",{t:"−0.64 ●",o:{color:GREEN}},"1.12","0.42",{t:"−0.70 ●",o:{color:GREEN}}],
   ["Latin America","46.59","32.80",{t:"−13.79 ●",o:{color:GREEN,bold:true}},"42.14","30.92",{t:"−11.22 ●",o:{color:GREEN,bold:true}}],
   ["Middle East","2.35","1.91",{t:"−0.44",o:{color:MUTE}},"0.86","2.06",{t:"+1.19 ●",o:{color:RED,bold:true}}],
   ["North America","6.38","4.93",{t:"−1.45 ●",o:{color:GREEN}},"5.05","4.06",{t:"−0.99",o:{color:MUTE}}],
   ["Reforming econ.","1.45","0.89",{t:"−0.56",o:{color:MUTE}},"1.15","1.17",{t:"+0.02",o:{color:MUTE}}],
   ["Rest of Asia","2.19","7.62",{t:"+5.42",o:{color:MUTE}},"3.10","6.81",{t:"+3.71 ●",o:{color:RED,bold:true}}]];
  const body=[H.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:9,fill:{color:PANEL},fontFace:F}}))];
  R.forEach(r=>body.push(r.map(c=>{const o={fontSize:10.5,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){Object.assign(o,c.o||{});return {text:c.t,options:o};} return {text:c,options:o};})));
  s2.addTable(body,{x:0.62,y:1.78,w:12.1,colW:[2.5,1.6,1.6,1.75,1.6,1.6,1.75],
    border:{type:"solid",color:LINE,pt:0.5},rowH:0.39,autoPage:false});
  s2.addText("Negative closes the gap and favours High-RE. Note the enormous range in baseline: Africa starts 50–59 GJ per capita short of decent living, China+ and India+ around 1. The two reversals (Middle East 2°C, Rest of Asia) do NOT survive the within-model test — see slide 26.",
    {x:0.62,y:6.55,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
}
{ const s2=slide(); head(s2,"Result — regions","PM2.5 mortality: deaths per 1,000, ammonia harmonised",{t:"8 / 20",c:MUTE});
  s2.addText("1.5°C HIGH AMBITION",{x:2.72,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  s2.addText("2°C MEDIUM AMBITION",{x:7.75,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  const H=["Region","High-CMT","High-RE","Difference","High-CMT","High-RE","Difference"];
  const R=[
   ["WORLD","26.21","26.29",{t:"+0.08",o:{color:MUTE,bold:true}},"28.01","27.26",{t:"−0.74",o:{color:MUTE,bold:true}}],
   ["Africa","23.05","23.39",{t:"+0.34",o:{color:MUTE}},"23.43","23.56",{t:"+0.13",o:{color:MUTE}}],
   ["China+","28.22","31.00",{t:"+2.78 ●",o:{color:RED,bold:true}},"31.07","32.49",{t:"+1.42 ●",o:{color:RED}}],
   ["Europe","8.76","8.49",{t:"−0.27",o:{color:MUTE}},"9.01","8.64",{t:"−0.37",o:{color:MUTE}}],
   ["India+","42.95","40.31",{t:"−2.63 ●",o:{color:GREEN,bold:true}},"46.40","43.66",{t:"−2.74",o:{color:MUTE}}],
   ["Latin America","8.32","9.17",{t:"+0.85",o:{color:MUTE}},"9.39","9.26",{t:"−0.13",o:{color:MUTE}}],
   ["Middle East","32.86","29.37",{t:"−3.48",o:{color:MUTE}},"25.42","29.96",{t:"+4.54",o:{color:MUTE}}],
   ["North America","7.36","10.07",{t:"+2.71 ●",o:{color:RED,bold:true}},"7.79","10.10",{t:"+2.31 ●",o:{color:RED,bold:true}}],
   ["Reforming econ.","24.66","34.06",{t:"+9.41 ●",o:{color:RED,bold:true}},"24.98","34.88",{t:"+9.89 ●",o:{color:RED,bold:true}}],
   ["Rest of Asia","28.49","28.73",{t:"+0.24",o:{color:MUTE}},"30.80","29.74",{t:"−1.05",o:{color:MUTE}}]];
  const body=[H.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:9,fill:{color:PANEL},fontFace:F}}))];
  R.forEach(r=>body.push(r.map(c=>{const o={fontSize:10.5,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){Object.assign(o,c.o||{});return {text:c.t,options:o};} return {text:c,options:o};})));
  s2.addTable(body,{x:0.62,y:1.78,w:12.1,colW:[2.5,1.6,1.6,1.75,1.6,1.6,1.75],
    border:{type:"solid",color:LINE,pt:0.5},rowH:0.39,autoPage:false});
  s2.addText("Negative favours High-RE. Twelve of twenty gaps are under one death per 1,000 on baselines of 7–46 — that is the null, expressed in units anyone can check. The four significant cells against High-RE are the ones the within-model test cannot support: North America and Reforming Economies have NO model holding both arms.",
    {x:0.62,y:6.55,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
}

img("Result — robustness","No single design choice carries the result","Y3_robustness.png",
  "Tercile cut, threshold sample, label basis, database and vetting — none flips the answer.",
  "Share of cells favouring High-RE under each alternative specification. Only SCI vetting moves the number meaningfully, and that is a power effect concentrated in deprivation: vetting cuts the sample from 590 to 137, so cells lose significance rather than changing direction.",
  {t:"ROBUST",c:GOLD},0.42);

table("Result — classification basis","Global tercile or per-region tercile? It barely matters.",
  ["","Global tercile (published)","Per-region tercile","What it means"],
  [[{t:"Labels that differ",o:{bold:true}},"—",{t:"3.6%",o:{bold:true}},"Of 3,248 labelled region-rows, 116 flip. A scenario leaning renewable globally leans renewable locally too"],
   ["Where the labels DO differ","—","Pacific OECD 74% same · Reforming econ. 91%","Exactly the two regions already flagged for weak label coherence — an internal consistency check that passes"],
   [{t:"Cells favouring High-RE",o:{bold:true}},{t:"38 of 54",o:{bold:true}},{t:"37 of 54",o:{bold:true}},"Nine regions, no World row (World IS the global aggregate, so no per-region label exists there)"],
   ["Significantly for / against","30 / 9","29 / 9","Indistinguishable"],
   ["Jobs","18/18","18/18","No cell changes sign"],
   ["Deprivation","14/18","12/18","Slightly weaker; two cells change sign"],
   ["Health","6/18","7/18",{t:"four cells change sign — Africa −0.29 → +0.48",o:{color:RED}}],
   [{t:"REMIND share of High-RE arm",o:{bold:true}},{t:"85%",o:{bold:true,color:RED}},{t:"84%",o:{bold:true,color:RED}},"THE POINT: per-region labelling does not touch the model-composition problem at all"],
   ["Scenarios labelled","590 (all regions)",{t:"~55% per region",o:{color:RED}},"The per-region rule discards roughly half the sample in each region, so the within-model test has far less to work with"]],
  [3.2,2.5,2.6,3.8],{t:"LABEL BASIS",c:GOLD},
  "The classification basis is not load-bearing: seven of 54 cells change sign, five of them health, and none of them jobs. The global tercile is retained because it answers the question the paper asks — do pathway ARCHETYPES differ — and because it is the only basis on which the World row is defined.",10);

table("Result — window sensitivity","Would running to 2100 strengthen the story? No.",
  ["","2020–2050 (primary)","2020–2100","What it means"],
  [[{t:"Cells favouring High-RE",o:{bold:true}},{t:"44 of 60",o:{bold:true}},{t:"39 of 60",o:{bold:true,color:RED}},"The longer window is worse overall"],
   ["Significantly against High-RE","9",{t:"13",o:{color:RED}},"Driven almost entirely by health"],
   [{t:"Jobs — cells",o:{bold:true}},"20/20","20/20","Unchanged, and gains one significant cell"],
   ["Jobs — median gap","+6.9 job-yr/1,000","+29.6 job-yr/1,000","Absolute gap grows; consistency falls"],
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
  [["Cells where High-RE is better","20/20","16/20","8/20"],
   ["Clearing the interval, for High-RE","19","12","1"],
   ["Clearing the interval, against","0","2","6"],
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
  "In raw units: the World gap falls from 1.68 to −0.08 deaths per 1,000 at 1.5°C.",
  "TM5-FASST re-run with ammonia removed for EVERY model, on the same 590 classified scenarios, with the data-quality gate held fixed at the original run so both versions cover identical scenarios. The re-cut reproduces the published mortality cells exactly when fed the original file — max |difference| in Cliff's delta 0.000 — which is what makes the harmonised numbers trustworthy.",
  {t:"CORRECTED",c:RED},0.42);

bullets("The correction — what to conclude",
  "The co-benefit does not survive, and the correction is the conservative one",
  ["THE AS-REPORTED RESULT WAS NEVER SIGNIFICANT EITHER. On the raw basis the World advantage is +1.67 [−0.12, +3.08] at 1.5°C and +3.02 [−0.55, +4.45] at 2°C — both straddle zero. Harmonising takes them to −0.08 and +0.74. So the two versions disagree about the point estimate and agree about the conclusion: air-quality mortality does not distinguish the pathways at World.",
   "THE PUBLISHED NUMBER IS THE MIDDLE OF THE RANGE, NOT THE PESSIMISTIC END. \"Putting ammonia back\" means two opposite things, and only one of them is a correction — see the next slide. Restoring the data as reported hands High-RE a 1.67 deaths-per-1,000 advantage at World; imputing the ammonia REMIND never filed hands it a 1.25 DISADVANTAGE. The harmonised figure sits between them at −0.08.",
   "NORTH AMERICA'S REMAINING GAP IS NOT A FINDING. 2.71 deaths per 1,000 on 47 versus 17 scenarios with NO model family holding both arms. Inside MESSAGEix the sign reverses. Reforming Economies (9.41) is in the same territory. Both are reported as unresolvable rather than as evidence that carbon management is healthier.",
   "THIS IS A CONTRIBUTION, NOT A LOSS. Anyone using AR6 for air-quality work will hit this, and nobody has written it down. The paper reports the naive estimate, the diagnosis and the correction — which is a stronger and more useful result than the co-benefit would have been."],
  {t:"CONCLUSION",c:RED});

table("Result — mortality","Reported both ways, so the reader meets the caveat in the table",
  ["Region","As reported","","Ammonia harmonised","","Does it survive?"],
  [[{t:"",o:{}},{t:"1.5\u00b0C",o:{bold:true,color:MUTE}},{t:"2\u00b0C",o:{bold:true,color:MUTE}},
    {t:"1.5\u00b0C",o:{bold:true,color:MUTE}},{t:"2\u00b0C",o:{bold:true,color:MUTE}},{t:"",o:{}}],
   [{t:"WORLD",o:{bold:true}},{t:"+1.67",o:{bold:true}},{t:"+3.02",o:{bold:true}},
    {t:"\u22120.08",o:{bold:true,color:MUTE}},{t:"+0.74",o:{bold:true,color:MUTE}},
    {t:"no \u2014 and it was never significant",o:{color:RED}}],
   ["Africa","\u22120.34","\u22120.04","\u22120.34","\u22120.13","unchanged"],
   ["China+","+3.03",{t:"+4.99 \u25cf",o:{color:GREEN}},{t:"\u22122.78 \u25cf",o:{color:RED}},
    {t:"\u22121.42 \u25cf",o:{color:RED}},{t:"no \u2014 reverses at both levels",o:{color:RED}}],
   ["Europe",{t:"+5.14 \u25cf",o:{color:GREEN}},{t:"+6.70 \u25cf",o:{color:GREEN}},"+0.27","+0.37",
    {t:"no \u2014 95% of it was ammonia",o:{color:RED}}],
   ["India+",{t:"+3.42 \u25cf",o:{color:GREEN}},"+3.75",{t:"+2.63 \u25cf",o:{color:GREEN}},"+2.74",
    {t:"YES",o:{color:GREEN,bold:true}}],
   ["Latin America","\u22120.62","+0.50","\u22120.85","+0.13","unchanged"],
   ["Middle East","+4.66","\u22122.11","+3.48","\u22124.54","unchanged, neither significant"],
   ["North America","\u22121.31","\u22120.60",{t:"\u22122.71 \u25cf",o:{color:RED}},
    {t:"\u22122.31 \u25cf",o:{color:RED}},"strengthens against High-RE"],
   ["Reforming econ.",{t:"\u22124.80 \u25cf",o:{color:RED}},"\u22124.83",
    {t:"\u22129.41 \u25cf",o:{color:RED}},{t:"\u22129.89 \u25cf",o:{color:RED}},"strengthens against High-RE"],
   ["Rest of Asia","+0.22","+1.54","\u22120.24","+1.05","unchanged"],
   [{t:"Cells favouring High-RE",o:{bold:true}},{t:"12 of 20",o:{bold:true}},{t:"",o:{}},
    {t:"8 of 20",o:{bold:true}},{t:"",o:{}},{t:"4 change sign, 2 lose significance",o:{bold:true}}]],
  [2.3,1.5,1.5,1.75,1.75,3.3],{t:"BOTH WAYS",c:GOLD},
  "Deaths per 1,000 avoided by High-RE; positive favours it. \u25cf clears a cluster-robust 95% interval on the raw difference. THE KEY LINE: on the raw basis the as-reported World advantage is +1.67 [\u22120.12, +3.08] and +3.02 [\u22120.55, +4.45] \u2014 neither clears zero. Even taken at face value, the World co-benefit was never a significant result once the interval is placed on the gap rather than on rank overlap.",10);

img("Result — mortality","What the mortality data does say clearly","Y12_burden.png",
  "A 4.7-fold gap in air-pollution burden between regions, which no pathway choice closes.",
  "The two arm medians sit on top of each other in every region; the regions do not. India+ carries 44.1 deaths per 1,000 against Latin America 9.3, North America 10.1 and Europe 11.7. The ordering is identical when ammonia is harmonised (India+ 43.6 against Europe 8.7, a 5.0-fold gap), so this finding is independent of the reporting question entirely.",
  {t:"BURDEN",c:GOLD},0.42);

table("The correction","\u201CPut the ammonia back\u201D means two opposite things",
  ["","What is done","High-CMT","High-RE","World gap 1.5\u00b0C","Cells for High-RE"],
  [[{t:"A · Revert",o:{bold:true,color:RED}},"Leave each model's ammonia as reported","28.20","26.52",
    {t:"+1.67",o:{bold:true,color:RED}},{t:"12 of 20",o:{color:RED}}],
   [{t:"",o:{}},{t:"NOT a correction. High-CMT keeps ~8% of its mortality from ammonia; High-RE keeps 0.4%, because REMIND never filed any. The gap IS the reporting difference.",o:{color:MUTE,fontSize:9.5}},"","","",""],
   [{t:"B · Harmonise",o:{bold:true,color:GREEN}},"Remove ammonia from every model","26.21","26.29",
    {t:"\u22120.08",o:{bold:true,color:GREEN}},{t:"8 of 20",o:{color:GREEN}}],
   [{t:"",o:{}},{t:"PUBLISHED. Both arms now on the same basis. Costs the four families that do report ammonia their real signal, but needs no external data and cannot be accused of assuming an answer.",o:{color:MUTE,fontSize:9.5}},"","","",""],
   [{t:"C · Impute",o:{bold:true,color:CMT}},"Give REMIND the ammonia it is missing","28.00","29.25",
    {t:"\u22121.25",o:{bold:true,color:CMT}},{t:"4 of 20",o:{color:CMT}}],
   [{t:"",o:{}},{t:"The scientifically correct fix, and the one that penalises High-RE MOST. Needs an external agricultural ammonia source; rests here on n=1 for the High-RE fraction, so it is a bound rather than a result.",o:{color:MUTE,fontSize:9.5}},"","","",""]],
  [1.75,4.55,1.25,1.25,1.6,1.7],{t:"THE RANGE",c:GOLD},
  "Deaths per 1,000, cumulative 2020\u20132050. The gap is deaths AVOIDED by High-RE, so positive favours it. The decisive input: in MESSAGEix, the only family holding both arms while reporting ammonia, the eight High-CMT runs sit at 5.2\u20136.6% ammonia and the single High-RE run at 10.1% \u2014 HIGHER. If BECCS-heavy carbon management genuinely carried more fertiliser ammonia we would see the reverse, so the gap is a filing difference, not a pathway property.",10);

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
   "FACTOR TWO — THE INCUMBENT FLEET. Correlation −0.50 with fossil capacity in 2020 and −0.62 with nuclear. The bigger the existing energy workforce, the more a renewable build has to overcome before it shows up as a NET gain. Reforming economies holds 2.50 TW of fossil capacity, the most of any region, and has the weakest jobs advantage — its 1.5°C interval, [−3.49, +15.24], is the only jobs cell in the study that fails to clear zero.",
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
   "THE DIVIDEND IS FRONT-LOADED, AND THAT IS A REAL LIMITATION. O&M — the part that persists after the build-out — is only 16% of the gap to 2050. A pathway that builds fast produces a labour surge, not a permanent workforce. Extending to 2100 shifts the composition exactly as the mechanism predicts: the build share falls from 87% to 75% and O&M rises from 16% to 25%, while the per-cell consistency falls — 83% of model families agree with the pooled direction to 2050, only 62% to 2100.",
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
  [["Jobs 1.5°C / 2°C","6.96→19.21 · 3.88→13.63",GREEN],
   ["Deprivation gap, GJ/cap","1.11→0.47 · 1.12→0.42",GREEN],
   ["Mortality /1,000","42.95 v 40.31 · 46.40 v 43.66"],
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
  [["Jobs 1.5°C / 2°C","1.80→4.67 · 1.38→3.64",GREEN],
   ["Deprivation gap, GJ/cap","4.60→2.01 · 2.92→1.92",GREEN],
   ["Mortality /1,000","8.76 v 8.49 · 9.01 v 8.64"],
   ["Mortality gap BEFORE fix","5.14 · 6.70 deaths",RED],
   ["Renewable build gap","+1.89"],
   ["Fossil jobs term","−0.80",RED],
   ["Nuclear jobs term","−0.57",RED],
   ["NH₃ effect, High-CMT","−36.0%",RED],
   ["NH₃ effect, High-RE","−0.7%"]],
  "Europe has the SMALLEST renewable build gap of any included region (+1.89) and still sweeps jobs, because the work is concentrated in manufacturing rather than raw capacity. It also has the largest simultaneous retirement of fossil (−0.92), nuclear (−0.57) and bioenergy (−0.48) employment, so the net advantage is earned against the stiffest incumbency headwind in the study.",
  "Europe is where the ammonia problem bites hardest: intensive livestock and fertiliser use make agricultural NH₃ a dominant PM2.5 precursor, so removing it cut High-CMT's modelled mortality by 36.0% and High-RE's by 0.7%. The apparent −36% health advantage was almost entirely a reporting difference. What remains (+0.21) is not significant. Europe's real health story in this window is about agriculture, not about power generation.",
  "Europe and China+ are the two regions where the global label holds most tightly on their own renewable deployment (Cliff's delta 0.72–0.87 on their own renewable deployment), so their results carry the least classification risk.");

region("Africa — the largest jobs gap and the largest deprivation stakes","4 / 6",GREEN,
  [["Jobs 1.5°C / 2°C","2.80→10.37 · 1.52→7.33",GREEN],
   ["Deprivation gap, GJ/cap","49.98→38.41 · 58.62→34.03",GREEN],
   ["Mortality /1,000","23.05 v 23.39 · 23.43 v 23.56"],
   ["Renewable build gap","+5.99",GREEN],
   ["Fossil capacity 2020","0.06 TW"],
   ["Baseline deprivation gap","71.9 GJ/cap",RED],
   ["Land-based share of CDR","49%"],
   ["Within-model depriv. 2C","−0.28",RED]],
  "Africa has essentially nothing to retire — 0.06 TW of fossil capacity, the smallest of any region — and a large build ahead of it, so the jobs advantage is the largest in the study at +326%. Its deprivation baseline is also by far the worst: a 71.9 GJ/cap shortfall against a 17 GJ/cap threshold, meaning most of the population is far below decent-living energy at the start of the window.",
  "The health result going mildly against High-RE is not surprising and not alarming: African PM2.5 is dominated by residential solid-fuel burning and mineral dust, neither of which the power-sector mix moves much in this window. The wellbeing lever in Africa is access to energy, not the cleanliness of the grid — and that is exactly what the deprivation measure captures.",
  "Caveat: at 2°C the within-model deprivation direction reverses to −0.28, so Africa is one of the cells where the pooled deprivation advantage cannot be attributed to the pathway. The jobs result is unaffected.");

region("China+ — sweeps on jobs and deprivation, loses on health","4 / 6",GOLD,
  [["Jobs 1.5°C / 2°C","6.20→10.14 · 2.18→7.24",GREEN],
   ["Deprivation gap, GJ/cap","1.13→0.75 · 0.76→0.59",GREEN],
   ["Mortality /1,000","28.22 v 31.00 · 31.07 v 32.49",RED],
   ["Renewable build gap","+3.17"],
   ["Fossil capacity 2020","1.64 TW",RED],
   ["Fossil jobs term","−0.16"],
   ["Within-model health","−0.19",RED],
   ["NH₃ effect, High-CMT","−18.5%"]],
  "China+ carries the second-largest incumbent energy workforce anywhere (1.64 TW of fossil capacity) and still sweeps jobs and deprivation, because the remaining build is larger still. Its fossil-jobs term is small (−0.16), so unlike Europe it is not winning against a heavy retirement — it is simply adding on top of an existing system.",
  "China+ is the one region where the health result goes significantly against High-RE and SURVIVES the within-model check (MESSAGEix −0.19 agrees with the pooled −0.50). That makes it the most credible pro-High-CMT health signal in the study. The plausible mechanism is that China's PM2.5 is heavily influenced by ammonium nitrate from agriculture combined with industrial NOₓ, and the High-RE scenarios there do not reduce the agricultural component while carrying different industrial trajectories.",
  "China+ was the only region to win all three families at 2°C before the ammonia correction. It now wins two. That change is entirely attributable to the correction, not to any change in the jobs or deprivation data.");

region("North America — jobs and deprivation hold, health is unresolvable","4 / 6",GOLD,
  [["Jobs 1.5°C / 2°C","4.88→9.28 · 2.94→7.15",GREEN],
   ["Deprivation gap, GJ/cap","6.38→4.93 · 5.05→4.06",GREEN],
   ["Mortality /1,000","7.36 v 10.07 · 7.79 v 10.10",RED],
   ["Mortality gap","+2.71 · +2.31 deaths",RED],
   ["Nuclear capacity 2020","0.25 TW",RED],
   ["Nuclear jobs term","−0.62",RED],
   ["Novel CDR share","25%",CMT],
   ["Within-model health","+0.32"],
   ["Health arm sizes","47 vs 17",RED]],
  "North America holds the highest incumbent nuclear capacity of any region (0.25 TW) and the largest nuclear-jobs penalty (−0.62), so its jobs advantage of +117% is earned against a substantial high-skill retirement. It also has the most technology-forward carbon-management portfolio, with novel CDR at 25% of its removal mix — the highest anywhere.",
  "The mortality cell shows High-RE at 10.07 deaths per 1,000 against High-CMT at 7.36 — a gap of 2.71 that clears its interval. But it rests on 47 versus 17 scenarios with NO model family holding both arms, and inside MESSAGEix the sign reverses in High-RE's favour. Every model that contains both arms disagrees with the pooled figure. This cell is reported as unresolvable and should not be cited as evidence that carbon management is healthier in North America.",
  "This is the clearest single illustration in the deck of why the within-model test exists. On the pooled numbers it looks like one of the largest effects in the study; it is the one least able to bear weight.");

region("Latin America — the land-based outlier","4 / 6",GOLD,
  [["Jobs 1.5°C / 2°C","6.65→12.19 · 3.57→10.69",GREEN],
   ["Deprivation gap, GJ/cap","46.59→32.80 · 42.14→30.92",GREEN],
   ["Mortality /1,000","8.32 v 9.17 · 9.39 v 9.26"],
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
  [["Jobs 1.5°C / 2°C","7.90→19.31 · 1.87→10.09",GREEN],
   ["Deprivation gap, GJ/cap","2.35→1.91 · 0.86→2.06",RED],
   ["Mortality /1,000","32.86 v 29.37 · 25.42 v 29.96"],
   ["Renewable share 2020","14%",RED],
   ["Renewable build gap","+8.89",GREEN],
   ["Fossil jobs term","+0.20",GREEN],
   ["Fossil CCS share of CDR","77%",CMT],
   ["Final energy, High-RE","+43%",GREEN],
   ["Within-model depriv. 2C","+0.22",GREEN]],
  "The Middle East has the lowest renewable share anywhere (14%) and the second-largest deployment gap (+8.89), so the jobs advantage is the second-largest in the study at +292%. Notably its fossil-jobs term is POSITIVE (+0.20 at 2°C): High-RE scenarios there retain MORE fossil employment than High-CMT and still win on jobs, which rules out the 'wins by shutting down coal' reading entirely.",
  "For the Gulf, High-CMT is not a climate strategy layered onto the economy — it IS the economy. At 77% fossil CCS, the highest share anywhere, carbon management means continuing to produce and export hydrocarbons while capturing the emissions. High-RE means abandoning the export base. That is a political-economy difference of a completely different order from anything in Europe or India, and it is the main reason to expect the modelled pathways to diverge from what will actually be attempted.",
  "The 2°C deprivation reversal — the gap WIDENS from 0.86 to 2.06 GJ per capita — does NOT survive the within-model check: AIM, MESSAGEix and WITCH all favour High-RE inside the model. This is a Simpson reversal driven by model composition, and the region should not be reported as a genuine deprivation loss.");

region("Rest of Asia — the largest deprivation reversal, and it is not real","3 / 6",GOLD,
  [["Jobs 1.5°C / 2°C","4.01→14.46 · 2.19→10.15",GREEN],
   ["Deprivation gap, GJ/cap","2.19→7.62 · 3.10→6.81",RED],
   ["Mortality /1,000","28.49 v 28.73 · 30.80 v 29.74"],
   ["Renewable build gap","+8.93",GREEN],
   ["Fossil jobs term","+0.09 / +0.18",GREEN],
   ["Final energy, High-RE","+44%",GREEN],
   ["Within-model depriv.","+0.43 / +0.74",GREEN],
   ["AIM / WITCH / REMIND","+0.79 / +0.74 / +0.43",GREEN]],
  "Rest of Asia has the third-largest deployment gap (+8.93), a POSITIVE fossil-jobs term at both ambition levels, and High-RE scenarios that deliver 44% MORE final energy per capita. On every mechanistic indicator it should be one of the strongest deprivation cases in the study. Instead the pooled deprivation gap is 7.62 against 2.19 GJ per capita at 1.5°C and 6.81 against 3.10 at 2°C — substantially WORSE under High-RE.",
  "That contradiction is the clearest Simpson's paradox in the paper. Inside AIM, WITCH and REMIND alike — every substantial model family that holds both arms — High-RE has the SMALLER gap. The pooled reversal happens because MESSAGEix reports very low deprivation gaps in this region (median 0.86 GJ/cap) and populates 118 of the High-CMT scenarios, while REMIND reports much higher gaps (median 6.8–8.3) and populates 217 of the High-RE ones. The arms are comparing models, not pathways.",
  "Reported as a reversal that the within-model evidence contradicts. It is also the single best argument in the deck for why the deprivation family carries a limitation rather than a claim.");

region("Reforming economies — the weakest case, and the reason why","3 / 6",RED,
  [["Jobs 1.5°C / 2°C","10.77→17.18 · 3.93→11.61",GREEN],
   ["Jobs interval, 1.5°C","[−3.49, +15.24]",MUTE],
   ["Deprivation gap, GJ/cap","1.45→0.89 · 1.15→1.17"],
   ["Mortality /1,000","24.66 v 34.06 · 24.98 v 34.88",RED],
   ["Fossil capacity 2020","2.50 TW",RED],
   ["Fossil jobs term","−2.49",RED],
   ["Nuclear jobs term","−1.35",RED],
   ["Bioenergy jobs term","+1.91",GREEN],
   ["Label coherence 1.5°C","+0.07",RED]],
  "Reforming economies holds the largest incumbent fossil fleet of any region at 2.50 TW, and pays the largest employment penalty for it: fossil −2.49 and nuclear −1.35, the biggest retirements in the study. Its renewable-deployment gap at 1.5°C is only +0.07, meaning the global label barely distinguishes the two arms there — it is the weakest label coherence of any region still in the regional display.",
  "Russia and Central Asia are where the transition is most obviously a net industrial loss under this measure, and the models reflect that. The bioenergy term is the only strongly positive one (+1.91), which suggests the High-RE pathways there lean on biomass rather than wind and solar — the one place in the study where the renewable build is not primarily a manufacturing story.",
  "Retained with a flag rather than dropped. Its mortality cell — 34.06 against 24.66 deaths per 1,000, a gap of 9.41 and the largest in the study — has the same problem as North America's and is equally unresolvable: no model family holds both arms, and inside MESSAGEix the sign favours High-RE.");

region("Pacific OECD — excluded, and why that is the right call","EXCLUDED",RED,
  [["Renewable deployment δ","−0.19 / −0.07",RED],
   ["Jobs 1.5°C / 2°C","3.31→4.06 · 2.27→3.58"],
   ["Deprivation gap, GJ/cap","4.31→4.83 · 2.99→4.23",RED],
   ["Mortality /1,000","30.84 v 12.51 · 34.50 v 12.72"],
   ["Final energy, High-RE","−29% / −39%",RED],
   ["Fossil jobs term","−0.21 / −0.53",RED],
   ["Status","World aggregate only"]],
  "Pacific OECD is the one region where the global classification does not describe local behaviour: High-RE scenarios build NO MORE renewables there than High-CMT scenarios do (δ −0.19 and −0.07). The contrast the paper claims to measure does not exist inside that region, so any outcome difference is measuring something other than the pathway.",
  "It is retained inside the World aggregate, because the World row is a sum over all ten regions and dropping a region there would change what 'World' means. It is removed from the regional rows only. This is a decision about display, not about data: nothing is discarded.",
  "Independently, Pacific OECD is one of two regions where the jobs decomposition shows the advantage coming through fossil destruction rather than renewable construction, and its High-RE arm delivers 29–39% LESS final energy per capita — a further sign the label is not tracking what it should there.");

// ============================== 8. CLOSE =====================================
section("Part eight","What we can claim","And what has to be said alongside it.");

bullets("The claim","Three sentences the paper can defend",
  ["ONE. In the AR6 scenario ensemble, high-renewable mitigation pathways deliver substantially more net energy employment than high-carbon-management pathways at the same climate ambition. At World, 6.05 against 13.44 job-years per 1,000 people at 1.5°C — a difference of +7.39 [+6.35, +9.13] — and 2.43 against 8.87 at 2°C. Better in 20 of 20 region × ambition cells, and it holds inside individual modelling frameworks (83% of model families, 20 of 22 cells). The mechanism is construction and manufacturing, which together are 93% of the gap.",
   "TWO. High-renewable pathways are also associated with substantially lower energy deprivation — the decent-living gap closes from 13.96 to 10.01 GJ per capita at World, 16 of 20 cells, robust to every specification tested. But this association cannot be separated from model composition: only 42% of model families agree with the pooled direction, and the within-model answer reverses in both directions depending on region. It is reported as an association with that limitation stated.",
   "THREE. There is no detectable difference in air-quality mortality between the two pathway types once ammonia is placed on a common accounting basis across models. At World the gap is −0.08 deaths per 1,000 with an interval of [−0.90, +1.19]. The apparent co-benefit in the uncorrected data — 1.68 deaths per 1,000 avoided — was an artefact of REMIND not reporting agricultural ammonia, and the correction used is the one most favourable to renewables.",
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
  ["IMPUTE AMMONIA FOR REMIND rather than deleting it for everyone. Deletion discards real signal from the four families that do report NH₃. On the one within-model fraction available, imputation moves the World gap to −1.25 deaths per 1,000, i.e. FURTHER against High-RE. It needs an external agricultural ammonia source and is a next-paper problem, but it is the scientifically correct fix and it would not rescue the co-benefit.",
   "RE_SPEC DEFINITION SENSITIVITY. The renewables axis currently excludes nuclear and biomass, for a principled reason — biomass is the substrate of BECCS, so a BECCS-heavy scenario would score on both axes. The master supports low_carbon and with_biomass alternatives; running all three would answer the reviewer question directly. Requires two re-runs on the machine holding the interpolated database.",
   "REGIONAL EMISSIONS PROVENANCE. Scenarios reporting emissions only at World have their regional detail filled in by population weight, which carries no pathway information. A test is written; if those scenarios concentrate in one arm the regional mortality cells need re-cutting on genuine regional data.",
   "BERGERO / STATE OF CDR SCENARIOS, parked deliberately. Adding a CDR-focused ensemble moves the tercile thresholds and reclassifies scenarios unrelated to it, so it changes the comparison rather than extending it."],
  {t:"OPEN",c:TEAL});

p.writeFile({fileName:"COMPASS_Paper1_final.pptx"}).then(()=>console.log("slides:",N));

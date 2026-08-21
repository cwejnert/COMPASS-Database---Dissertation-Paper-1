// =============================================================================
// COMPASS Paper 1 — final deck.
// Diagnostics -> Methodology -> Results -> Mechanism -> Why -> Regions.
//
// Nine regions plus World. Pacific OECD is out of the regional display (the
// global label does not describe it) and stays inside the World aggregate.
// Every number comes from FINAL_RESULTS.rds via Z4_final_table.R; figures Y1-Y7
// come from Y1_final_figs.R off the same file, so deck and figures agree.
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
  s.addText("Wellbeing outcomes across nine world regions and two levels of ambition",
    {x:0.9,y:4.15,w:10.6,h:0.5,fontSize:16,color:"C7D6E0",fontFace:F});
  s.addShape(p.ShapeType.rect,{x:0.9,y:4.85,w:2.2,h:0.03,fill:{color:GOLD}});
  s.addText("590 scenarios · 24 models · 3 outcome families · 60 comparisons · cumulative 2020–2050",
    {x:0.9,y:5.1,w:10,h:0.34,fontSize:12,color:"9FB6C4",fontFace:F});
}

bullets("The answer in one slide","What the paper finds",
  ["High-renewable pathways deliver better wellbeing outcomes in 48 of 60 comparisons (80%) — three outcome families, nine regions plus World, both levels of ambition. 38 clear a cluster-robust interval; 6 go the other way.",
   "ENERGY JOBS is unanimous: 20 of 20 cells, 19 significant, none against — and identical under SCI vetting. Median gap +194% at World. It is a construction and manufacturing dividend, largest where there is most left to build.",
   "ENERGY DEPRIVATION follows in 16 of 20 cells, 14 significant, closing the gap by a median 30% at World. It holds in the full database and weakens but does not reverse under vetting.",
   "AIR-QUALITY MORTALITY leans the same way (12 of 20) but is NOT YET REPORTABLE: the models that populate the two arms do not report ammonia on the same basis. One harmonised re-run settles it.",
   "One region, Pacific OECD, is excluded from the regional results because the global label does not describe it — High-RE builds no more renewables there. It remains inside the World aggregate."],
  {t:"SUMMARY",c:GOLD},
  "Everything cumulated 2020–2050, the net-zero window. Significance throughout is a cluster bootstrap over 312 model × scenario-family clusters, not a scenario-level p-value.");

// ============================== 1. DIAGNOSTICS ===============================
section("Part one","Diagnostics","What is in the database, and what can it carry?");

table("The sample","What survives filtering and classification",
  ["","A — full database","C — SCI 2025 vetted"],
  [["Scenarios with R10 detail and an ambition class","1,425","—"],
   [{t:"Classified into a pathway",o:{bold:true}},{t:"590",o:{bold:true}},{t:"137",o:{bold:true}}],
   ["  High-CMT / High-RE","335 / 255","74 / 63"],
   ["Reporting all ten regions","537  (91%)","132  (96%)"],
   ["Models / model families","24 / 12","15 / 7"],
   [{t:"Independent model × scenario-family clusters",o:{color:RED}},{t:"312  (design effect 1.9×)",o:{bold:true,color:RED}},"—"],
   ["Passing the mortality precursor gate","501  (85%)","135  (99%)"],
   ["Region × ambition × family cells","60","60"]],
  [5.0,3.55,3.55], {t:"DIAGNOSTIC",c:TEAL},
  "Of the scenarios reporting BOTH classification axes, 44% at 2°C and 37% at 1.5°C fall in the excluded middle or the both-high corner. This is a contrast between two CORNERS, not a dose–response. Median 2°C deployment: High-CMT 647k carbon management / 1.60M renewables; High-RE 100k / 3.13M.");

img("Model composition","The two arms are not balanced",
  "Q3_model_share.png",
  "REMIND-MAgPIE supplies 59% of ALL High-RE scenarios and none of the High-CMT. Only 7 of 12 families hold both arms.",
  "This is a property of the database, not a flaw in the method — but a pooled comparison can pick up differences between MODELS and report them as differences between pathways. Two defences run throughout: every interval is bootstrapped over model × scenario-family clusters rather than over scenarios, and every family is checked against its within-model variance share before any pooled claim is made.",
  {t:"DIAGNOSTIC",c:TEAL}, 0.44);

img("Region screening","One region cannot carry the comparison",
  "Y5_label_coherence.png",
  "Pacific OECD: High-RE builds NO MORE renewables there than High-CMT (δ = −0.19 and −0.07).",
  "The classification axes are global sums, but the outcomes are regional, so the label has to describe the region it scores. In eight of ten regions it does, strongly (δ 0.72–0.99 on regional renewable deployment, −0.75 to −1.00 on regional carbon management). Pacific OECD is the exception and is dropped from the regional results. Reforming economies is weak but real and is kept, flagged.",
  {t:"DIAGNOSTIC",c:TEAL}, 0.46);

img("Poolability","Which outcomes can be compared across models at all",
  "Y7_variance.png",
  "Jobs 30–63% and deprivation 24–64% clear the 10% floor everywhere. Mortality falls below it in four of nine regions.",
  "Share of each outcome's variance that sits WITHIN a model family. Below 10% the two arms are barely observed inside the same model, so a pooled comparison reads the distance between model inventories as a pathway effect. This is the structural reason mortality cannot carry a pooled claim — and it is the test that jobs and deprivation pass.",
  {t:"DIAGNOSTIC",c:TEAL}, 0.46);

// ============================== 2. METHODOLOGY ===============================
section("Part two","Methodology","Six steps. Nothing depends on anything not listed here.");

table("Method","The whole design on one slide",
  ["","Step","Choice made"],
  [["1",{t:"Sample",o:{bold:true}},"All COMPASS / AR6 scenarios with R10 detail. Ambition from the AR6 climate category: C1+C2 → 1.5°C (high), C3+C4 → 2°C (medium)"],
   ["2",{t:"Two axes",o:{bold:true}},"Cumulative carbon management (land CDR + novel CDR + fossil CCS) and cumulative renewable capacity (solar, wind, hydro, geothermal), each summed over the ten R10 regions"],
   ["3",{t:"Classify",o:{bold:true}},"Within each ambition class, top tercile on one axis and NOT the other → High-CMT (335) or High-RE (255). High on both, or neither, is excluded. ONE fixed global set, applied unchanged in every region"],
   ["4",{t:"Outcomes",o:{bold:true}},"Three families, all cumulated 2020–2050. Jobs (renewables − fossil) · energy deprivation (cumulative gap, GJ/cap) · health (PM2.5 mortality via TM5-FASST). A second measure in each family serves as a within-family check"],
   ["5",{t:"Test",o:{bold:true}},"Cliff's delta, signed so positive ALWAYS means High-RE is better. Interval from a cluster bootstrap over 312 model × scenario-family clusters. Direction is the finding; the interval is the check"],
   ["6",{t:"Screen",o:{bold:true}},"Drop any region where the global label does not describe local deployment, and any family whose within-model variance share falls below 10%"]],
  [0.5,2.0,9.6], {t:"METHOD",c:TEAL},
  "Why 2020–2050: it is the window in which net zero is supposed to be reached, so it is the horizon the comparison is about. Each outcome's re-cut was verified to reproduce the master EXACTLY at 2020–2100 before the window was shortened, so the window is a parameter rather than a fork in the pipeline.");

bullets("Method — why three families, not five",
  "Two measures were being counted twice",
  ["The two jobs measures — renewables minus fossil, and low-carbon minus fossil — correlate ρ = 0.97 within region.",
   "The two deprivation measures — cumulative gap and headcount — correlate ρ = 0.99.",
   "Counting all five treats one result as two and inflates the denominator. The headline counts THREE families on one primary measure each; the second measure is reported as a within-family check and agrees in every case.",
   "Significance is cluster-robust throughout. 590 scenarios sit in only 312 model × scenario-family clusters — thirty REMIND-MAgPIE ENGAGE-NPi2020 variants are not thirty independent draws. Bootstrapping over clusters costs 13 of 98 significant cells and gains none.",
   "Per-capita denominators use a fixed base-period population identical in every scenario, so they rescale levels and cannot touch any contrast — Cliff's delta on raw totals equals delta per capita to 0.000."],
  {t:"METHOD",c:TEAL},
  "Robustness run on every choice: tercile cut from the top half to the top quarter · common-support thresholds · per-region against global labels · full database against SCI vetting · matched against unmatched samples.");

table("Method — outcome 1 of 3","Energy jobs: how the number is built",
  ["","",""],
  [[{t:"Source",o:{bold:true}},"AR6 job-intensity factors","job_factors_complete.csv, Rutovitz-style: a global employment factor per technology times a regional labour multiplier. Geothermal is absent upstream and is imputed at 1,170 jobs/GW O&M and ~20,400 jobs/GW build, carrying the mean regional multiplier"],
   [{t:"Three streams",o:{bold:true}},"construction · manufacturing · O&M","Construction and manufacturing scale with CAPACITY ADDITIONS (per GW built); operations and maintenance scales with CAPACITY STOCK (per GW installed). Extraction and refining attach to fossil fuel throughput"],
   [{t:"Technology groups",o:{bold:true}},"Renewables / Nuclear / Bioenergy / Fossil","Solar PV, onshore wind, hydro, geothermal → Renewables. Coal, gas, oil → Fossil. The renewable group is tied to RE_SPEC, so jobs_Renewables always matches how High-RE was defined — no drift between the axis and the outcome"],
   [{t:"The contrast",o:{bold:true}},{t:"renewables − fossil",o:{bold:true}},"A NET POSITION, not a displacement claim. The second measure, (renewables + bioenergy + nuclear) − fossil, is the within-family check and agrees in every cell"],
   [{t:"Window & units",o:{bold:true}},"2020–2050, job-years per 1,000","Four decadal snapshots x10 and summed — rectangle integration, the same convention mortality uses. The x10 is a pure relabelling: it changes no Cliff's delta and no percentage gap, only the units"],
   [{t:"Known limit",o:{bold:true}},{t:"partly transmission from the ranking axis",o:{color:RED}},"ρ(global RE axis, regional jobs contrast) = 0.40–0.90. Not a tautology — the axis is global, the outcome regional, and jobs-per-capacity varies 31–59% within region — but the CDR axis is anti-correlated at −0.48 to −0.62, so it is not a clean control either"]],
  [1.9,2.6,7.6], {t:"METHOD",c:TEAL},
  "No offshore wind category exists in the database, so all wind is onshore. Employment factors decline over time with learning, so the same GW built in 2050 carries fewer jobs than in 2020 — which is why the advantage is front-loaded.", 10);

table("Method — outcome 2 of 3","Energy deprivation: how the number is built",
  ["","",""],
  [[{t:"Threshold",o:{bold:true}},"Kikstra et al. 2021, fig 1A","Regional decent-living final-energy thresholds in GJ/cap/yr: India+ 10, China+ 15, Africa 17, Europe 28, North America 37. Global mean 17. Published REGIONAL values are used rather than rescaling to the DESIRE global mean of 22.3"],
   [{t:"Sector split",o:{bold:true}},"residential/commercial · transport · industry","DESIRE shares 6.7 / 11.8 / 3.8, normalised. The threshold is applied per sector, not just to the total"],
   [{t:"Efficiency path",o:{bold:true}},"−1.9%/yr, floored at 50%","Service-provisioning efficiency improves over time, so the energy needed to reach decent living falls. Lands at ~−38% by 2040, matching DESIRE's −30 to −46%. A floor at 50% prevents the threshold collapsing late in the century"],
   [{t:"The gap",o:{bold:true}},{t:"max(0, threshold − actual)",o:{bold:true}},"Truncated at zero: a region above its threshold has NO surplus credited. So the measure is deprivation, not net adequacy — a region cannot offset another's shortfall"],
   [{t:"The contrast",o:{bold:true}},"cumulative gap, GJ/capita","Summed over 2020–2050 from a TRULY ANNUAL series (31 values, not decadal). The second measure, the deprivation headcount, is a MEAN over the window rather than a sum — a stock, not a flow — and correlates ρ = 0.99"],
   [{t:"Known limit",o:{bold:true}},{t:"unverifiable within models",o:{color:RED}},"No model family holds enough of both arms to check the result inside a model: at 1.5°C exactly one family qualifies, carrying 100% of the stratified weight. Report the pooled result with that limitation stated"]],
  [1.9,2.6,7.6], {t:"METHOD",c:TEAL},
  "The regional Gini implied by these thresholds is understated relative to observed inequality (Africa 0.33 against ~0.43), so within-region distribution is not represented — the gap is a regional aggregate, not a household count.", 10);

table("Method — outcome 3 of 3","PM2.5 mortality: how the number is built",
  ["","",""],
  [[{t:"Model",o:{bold:true}},"TM5-FASST via rfasst","A source–receptor model: regional precursor emissions map to regional PM2.5 concentrations through fixed transfer coefficients, then to premature deaths"],
   [{t:"Precursors",o:{bold:true}},"SO₂ · NOx · BC · OM · NH₃ · VOC","Organic carbon is converted to organic matter at 1.3. Methane and CO enter the ozone module only. Emissions are disaggregated from R10 to 56 FASST regions by population weight, then aggregated back"],
   [{t:"Response function",o:{bold:true}},{t:"FUSION",o:{bold:true}},"rfasst returns three concentration–response functions — GBD, GEMM and FUSION. The pipeline uses FUSION, which sits between the other two. There is no single mortality column; naming the CRF is mandatory"],
   [{t:"Window & units",o:{bold:true}},"2020–2050, deaths per 1,000","Decadal values × 10 (rectangle integration), summed, divided by base-period population. Verified four ways that deaths_pm25 is an ANNUAL RATE, so the ×10 is right: 2020 global 5.86 mln/yr, smooth 5.7→7.95 rise, plausible per-capita rates, and the rfasst summary equals a plain sum at ratio 1.0000"],
   [{t:"Quality gate",o:{bold:true}},"≥6 non-zero precursors, all ten regions","501 of 590 classified scenarios pass, and the gate is BALANCED — 85% of both arms. COFFEE (0/12) and TIAM-ECN (0/23) are removed entirely"],
   [{t:"Validation",o:{bold:true}},{t:"6.9 mln deaths/yr globally",o:{color:GREEN}},"IQR 6.4–7.5, against GBD 4.1 and GEMM 8.9. Regional rates a consistent 1.4–1.6× GBD. The model is fine; the emissions going into it are the problem"],
   [{t:"Known limit",o:{bold:true}},{t:"ammonia is not reported on a common basis",o:{color:RED}},"NH₃ is 6–12% of PM2.5 mortality in IMAGE, POLES-JRC, AIM and MESSAGEix — and 0.15% in REMIND, which is ~95% of High-RE. High-CMT therefore carries ~9% extra mortality that is an accounting difference between modelling teams"]],
  [1.9,2.6,7.6], {t:"METHOD",c:TEAL},
  "Mortality also fails the poolability screen in four of nine regions, so even after ammonia is harmonised it would carry a within-model caveat. It is the one family reported with a hold rather than a result.", 9.5);

// ============================== 3. RESULTS ===================================
section("Part three","Results","Global first, then the regions that carry a story.");

img("Result","High-RE wins 48 of 60 comparisons",
  "Y1_scorecard.png",
  "80% of cells favour High-RE. 38 clear a cluster-robust interval; 6 go the other way. Jobs is unanimous.",
  "Jobs 20/20, deprivation 16/20, health 12/20. World is gold on all three families at 1.5°C and on jobs and deprivation at 2°C. The health column is the one that splits — and Part four explains why it cannot yet be read as a pathway result.",
  {t:"RESULT",c:GOLD}, 0.50);

table("Result — global","World, both levels of ambition",
  ["Family","Ambition","High-CMT","High-RE","Difference","Cliff's δ  [95% CI]"],
  [[{t:"Energy jobs",o:{bold:true}},"1.5°C","60.5","134.4",{t:"+122%",o:{bold:true,color:GOLD}},{t:"+0.96  [0.91, 1.00]",o:{bold:true}}],
   [{t:"Energy jobs",o:{bold:true}},"2°C","24.3","88.7",{t:"+265%",o:{bold:true,color:GOLD}},{t:"+0.92  [0.86, 0.96]",o:{bold:true}}],
   [{t:"Deprivation gap",o:{bold:true}},"1.5°C","13.96","10.01",{t:"−28%",o:{bold:true,color:GOLD}},{t:"+0.38  [0.12, 0.67]",o:{bold:true}}],
   [{t:"Deprivation gap",o:{bold:true}},"2°C","13.83","9.56",{t:"−31%",o:{bold:true,color:GOLD}},{t:"+0.32  [0.04, 0.53]",o:{bold:true}}],
   [{t:"PM2.5 mortality",o:{color:MUTE}},{t:"1.5°C",o:{color:MUTE}},{t:"28.20",o:{color:MUTE}},{t:"26.52",o:{color:MUTE}},{t:"−5.9%",o:{color:MUTE}},{t:"+0.47  [0.13, 0.82]",o:{color:MUTE}}],
   [{t:"PM2.5 mortality",o:{color:MUTE}},{t:"2°C",o:{color:MUTE}},{t:"30.76",o:{color:MUTE}},{t:"27.74",o:{color:MUTE}},{t:"−9.8%",o:{color:MUTE}},{t:"+0.33  [−0.05, 0.76]  n.s.",o:{color:MUTE}}]],
  [2.6,1.2,1.6,1.6,1.7,3.4], {t:"RESULT",c:GOLD},
  "Jobs in job-years per 1,000 people (decadal values x10, rectangle integration, matching the mortality convention); deprivation in GJ per capita; mortality in deaths per 1,000. All cumulative 2020–2050. Mortality is greyed because the two arms are not yet on the same precursor accounting — see Part four. Under a naive Wilcoxon every one of these six cells is significant, which is precisely why the cluster bootstrap is used instead.", 10.5);

img("Result — global","Nine of ten World cells clear the interval",
  "Y2_world_forest.png",
  "The jobs intervals sit almost entirely above +0.85. Deprivation is solid but wider. 2°C mortality is the one cell that does not clear zero.",
  "Effect sizes differ in kind, not just size: Cliff's delta near 1.0 on jobs means almost every High-RE scenario beats almost every High-CMT one, while +0.33 on mortality means a modest majority does. Reporting the percentage gap alongside the delta keeps that distinction visible.",
  {t:"RESULT",c:GOLD}, 0.42);

table("Result — robustness","Does any single choice carry the answer?",
  ["Family","Full DB, all","Full DB, matched","SCI-vetted, all","SCI-vetted, matched"],
  [[{t:"Energy jobs",o:{bold:true}},{t:"20 / 20",o:{bold:true,color:GOLD}},{t:"20 / 20",o:{bold:true,color:GOLD}},{t:"20 / 20",o:{bold:true,color:GOLD}},{t:"20 / 20",o:{bold:true,color:GOLD}}],
   [{t:"Deprivation",o:{bold:true}},{t:"16 / 20",o:{bold:true}},{t:"17 / 20",o:{bold:true}},"13 / 20","13 / 20"],
   [{t:"PM2.5 mortality",o:{color:MUTE}},{t:"12 / 20",o:{color:MUTE}},{t:"12 / 20",o:{color:MUTE}},{t:"10 / 20",o:{color:MUTE}},{t:"10 / 20",o:{color:MUTE}}],
   [{t:"ALL THREE",o:{bold:true}},{t:"48 / 60  (80%)",o:{bold:true,color:GOLD}},{t:"49 / 60  (82%)",o:{bold:true,color:GOLD}},{t:"43 / 60  (72%)",o:{bold:true}},{t:"43 / 60  (72%)",o:{bold:true}}]],
  [2.8,2.3,2.4,2.3,2.3], {t:"RESULT",c:GOLD},
  "JOBS IS COMPLETELY UNMOVED BY VETTING — 20 of 20 in all four samples. Deprivation weakens from 16/20 to 13/20 but does not reverse, and the vetted sample rests on 137 scenarios against 590, so it loses power as well as changing cells. The direction agrees between the two databases in 100% of jobs cells and 86% of deprivation cells.", 11);

img("Result — robustness","No single design choice carries the result",
  "Y3_robustness.png",
  "Moving the tercile from the top half to the top quarter moves the answer three points. Per-region labels instead of global move it two cells of twenty.",
  "Four families of alternative tested: where the cut sits, which sample the thresholds are computed on, whether labels are assigned globally or per region, and which database. Only SCI vetting moves the answer meaningfully, and that is a power effect concentrated in deprivation. What relaxation costs is significance, not direction.",
  {t:"RESULT",c:GOLD}, 0.46);

// ============================== 4. MORTALITY =================================
section("Part four","The mortality caveat","Why one family is held back, and what fixes it.");

img("Why mortality waits","The two arms are not on the same accounting basis",
  "Y6_nh3_gap.png",
  "IMAGE 12.4%, POLES-JRC 9.4%, AIM 8.9%, MESSAGEix-GLOBIOM 6.4% — against REMIND-MAgPIE 0.16% and REMIND 0.14%. A 58-fold gap.",
  "Agricultural ammonia is roughly 85% of global NH3 and drives ammonium nitrate and sulfate, a large part of PM2.5. In REMIND that agriculture lives in MAgPIE, so it never reaches Emissions|NH3 — and REMIND is ~95% of the High-RE arm. High-CMT therefore carries about 9% extra mortality that is an accounting difference between modelling teams, not a consequence of its pathway, and it flatters High-RE. The one High-RE run inside a family that also holds High-CMT sits with the blue points, not the gold ones, which is what a reporting artefact rather than a pathway property looks like.",
  {t:"CAVEAT",c:RED}, 0.44);

bullets("Why mortality waits","What it takes to settle it",
  ["The fix is to put both arms on the same basis: re-run TM5-FASST with ammonia excluded for EVERY model. That understates absolute PM2.5 uniformly but leaves the pathway contrast unbiased. One overnight run.",
   "Expect the advantage to shrink or vanish. On a 37-scenario test, harmonising moved High-RE from 5 of 10 regional cells to 2 of 10.",
   "That would itself be a reportable finding: once precursor accounting is harmonised, the air-quality co-benefit of renewables over carbon management is not detectable in this ensemble. It is a result, not a hole.",
   "The better fix scientifically is to IMPUTE ammonia for REMIND rather than delete it from everyone, since deleting discards real signal from the four families that do report it. That needs an external ammonia source and is a next-paper problem.",
   "Independently of ammonia, mortality also fails the poolability screen in four of nine regions, so it would carry a within-model caveat even after the re-run."],
  {t:"CAVEAT",c:RED},
  "TM5-FASST itself validates: global 6.9 mln deaths/yr (IQR 6.4–7.5) against GBD 4.1 and GEMM 8.9, with regional rates a consistent 1.4–1.6× GBD. The problem is the emissions going in, not the model processing them.");

// ============================== 5. MECHANISM =================================
section("Part five","Mechanism","Why the jobs and deprivation results look the way they do.");

img("Mechanism","Building, not demolishing",
  "Y4_jobs_decomposition.png",
  "In India+, Rest of Asia and the Middle East, High-RE wins WHILE RETAINING MORE fossil workers.",
  "Splitting the contrast into its two terms separates two different stories. In most regions High-RE wins because it builds more labour-intensive capacity, and in three regions it does so with no fossil job loss at all — the strongest form of the result. In the Reforming economies at 1.5°C the renewable-jobs delta is only +0.07, so that win is mostly fossil job destruction and should not be presented as an employment gain.",
  {t:"MECHANISM",c:TEAL}, 0.46);

img("Mechanism","A construction and manufacturing dividend",
  "P4a_jobtype_region.png",
  "Manufacturing +2.3, construction +1.5, operations and maintenance +1.1, extraction −0.2 job-years per 1,000.",
  "The pattern holds in every region without exception. Extraction is the only category where High-CMT leads — fossil CCS keeps the fuel supply chain running, which is exactly the employment a renewable substitution retires. The O&M term is the part that persists after the build-out, and it is real but much smaller than the headline.",
  {t:"MECHANISM",c:TEAL}, 0.42);

img("Mechanism","Two numbers order the regions",
  "P2b_two_factor.png",
  "How much is left to build (ρ = +0.67) against how much has to be retired (ρ = −0.62 nuclear, −0.50 fossil).",
  "India+ has the largest remaining build on Earth and almost nothing to retire, and posts the largest advantage anywhere. The Reforming economies carry the heaviest incumbent workforce and post the narrowest. Nothing else is needed to order the regions.",
  {t:"MECHANISM",c:TEAL}, 0.48);

img("Mechanism","What High-CMT actually means, region by region",
  "P2a_cmt_composition.png",
  "Middle East 77% fossil CCS. Latin America 73% land-based CDR. World 43% / 40%.",
  "The carbon-management axis changes character across the map, and that drives the regional spread. Where fossil CCS dominates, High-CMT keeps the fossil fleet and its workforce, so the jobs contrast is a genuine substitution. Where land-based CDR dominates it is afforestation, which creates no energy jobs at all — so in Latin America the jobs result is closer to definitional and should be presented as such.",
  {t:"MECHANISM",c:TEAL}, 0.46);

// ============================== 6. WHY =======================================
section("Part six","Why","Is the mechanism credible outside the models?");

bullets("Why — real-world plausibility","The mechanism is an engineering fact, not a modelling artefact",
  ["Solar and wind are capital- and manufacturing-intensive. Labour is spread across many small installation sites and the cost is almost entirely up front.",
   "Fossil generation is fuel-cost-intensive, with labour concentrated in extraction. Adding capture raises capital cost substantially but adds little labour beyond the host plant.",
   "So substituting renewable capacity for fossil-plus-capture SHOULD move employment out of extraction and into manufacturing and construction. That is the decomposition we measure, not an assumption imposed on it.",
   "It predicts the timing: the gain is front-loaded in the build-out, so 2020–2050 shows a larger gap than the full century does.",
   "It predicts the exceptions: mature systems with little left to build and large incumbent fleets are where the advantage should narrow. Europe, the Reforming economies and Pacific OECD are exactly those, and they are exactly where it narrows.",
   "And it predicts deprivation: more delivered energy per unit of mitigation effort closes the decent-living gap, which is what 16 of 20 cells show."],
  {t:"WHY",c:GOLD},
  "The honest limit: much of the jobs advantage is a transitional construction dividend. The operations-and-maintenance component, +1.1 job-years per 1,000, is what persists after the build-out.");

bullets("Why — the exceptions","Every cell that favours High-CMT has a reason",
  ["REST OF ASIA loses both deprivation cells (δ −0.41, −0.51) — the only region to do so. Its High-CMT scenarios deliver more final energy per capita, so the decent-living gap closes faster under carbon management despite the jobs result going the other way (+0.98).",
   "MIDDLE EAST loses deprivation at 2°C (−0.39). It is the purest fossil-CCS region at 77%, so High-CMT there means keeping the fossil fleet running — which delivers energy as well as emissions.",
   "REFORMING ECONOMIES ties on deprivation at 2°C (−0.01, not significant) and posts the weakest jobs advantage. It carries the heaviest incumbent energy workforce of any region — this is the just-transition case, arrived at from the other direction.",
   "NORTH AMERICA is the only region where mortality reverses significantly at both ambition levels. It also has the highest incumbent nuclear capacity anywhere, and every model holding both arms puts High-RE lower — the pooled figure disagrees with all of them.",
   "The mortality reversals in AFRICA and LATIN AMERICA are not significant and sit in regions where under 4% of mortality variance is within-model. They should be reported as untestable, not as findings."],
  {t:"WHY",c:GOLD},
  "Four of the five deprivation reversals are real and regional. They should be explained, not explained away — a paper that reports 20/20 everywhere is less credible than one that can say where and why the mechanism runs out.");

// ============================== 7. REGIONS ===================================
section("Part seven","The regions that carry a story","Six regions plus World, and what each one shows.");

table("Regions","One line each",
  ["Region","Jobs","Depriv.","Health","Median gap (jobs / depriv.)","What it shows"],
  [[{t:"WORLD",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"2/2",o:{bold:true,color:GOLD}},"2/2","+194% / +30%","The headline. Ten-region sum, restricted to scenarios reporting every region"],
   [{t:"India+",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"2/2",o:{bold:true,color:GOLD}},"2/2","+214% / +60%","The strongest case anywhere: largest remaining build on Earth, almost nothing to retire, and it wins with ZERO fossil job loss"],
   [{t:"Africa",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"0/2",o:{color:CMT}},"+326% / +33%","Largest jobs gap in the study and a clear deprivation win — nothing to defend, everything to build. Mortality goes the other way but is not significant"],
   [{t:"Europe",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"2/2",o:{bold:true,color:GOLD}},"2/2","+161% / +45%","The hard case that still wins: joint-smallest remaining build, losses on three fronts at once — fossil, nuclear and bioenergy — and it wins every family at both levels"],
   [{t:"China+",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"2/2",o:{bold:true,color:GOLD}},"2/2","+148% / +28%","Second-largest incumbent workforce and still sweeps, because the remaining build is larger still"],
   [{t:"Rest of Asia",o:{bold:true}},{t:"2/2",o:{bold:true,color:GOLD}},{t:"0/2",o:{bold:true,color:CMT}},"2/2","+312% / −184%","The interesting tension: the largest jobs advantage sits alongside the only double deprivation loss. Jobs and energy access are not the same claim"],
   [{t:"Reforming econ.",o:{bold:true}},{t:"2/2",o:{color:GOLD}},{t:"1/2",o:{color:MUTE}},{t:"0/2",o:{color:CMT}},"+127% / +19%","The just-transition case. Heaviest incumbent workforce anywhere; its jobs win is mostly fossil job destruction, not renewable job creation"],
   [{t:"Pacific OECD",o:{color:RED}},{t:"—",o:{color:RED}},{t:"—",o:{color:RED}},{t:"—",o:{color:RED}},{t:"excluded",o:{color:RED}},{t:"High-RE builds NO MORE renewables here, so the contrast does not exist. Kept inside the World sum only",o:{color:RED}}]],
  [1.8,0.7,0.8,0.8,2.1,5.9], {t:"REGIONS",c:GOLD},
  "Latin America, Middle East and North America are omitted from this slide for space, not for weakness — all three appear in the full grid. Latin America's High-CMT is 73% land-based CDR (afforestation creates no energy jobs, so its jobs result is close to definitional); Middle East is 77% fossil CCS and loses deprivation at 2°C; North America is the only region reversing on mortality at both ambition levels.", 9.5);

bullets("Regions — the high-build economies","India+, Africa, Rest of Asia: most to build, least to retire",
  ["INDIA+ is the strongest case in the study. Jobs +214% median, deprivation +60%, and it wins on renewable job CREATION (δ +0.89) with a fossil-jobs delta of 0.00 — the transition adds without subtracting. Its decent-living threshold is the lowest of any region at 10 GJ/cap, so the same delivered energy closes proportionally more of the gap.",
   "AFRICA posts the largest jobs gap anywhere, +326%, on almost no incumbent workforce — there is nothing to defend and everything to build. Its High-CMT is 49% land-based CDR, which is afforestation and creates no energy jobs at all, so the contrast is unusually stark.",
   "REST OF ASIA is the study's most instructive tension: the second-largest jobs advantage (+312%) sits alongside the ONLY double deprivation loss (δ −0.41 and −0.51). Its High-CMT scenarios deliver more final energy per capita, so the decent-living gap closes faster under carbon management even as the labour case runs the other way.",
   "That tension is the paper's most useful regional finding. Jobs and energy access are not the same claim, and a region can be better off on employment while worse off on delivered energy. Any policy read that treats 'renewables are better' as a single fact fails here.",
   "Mechanically all three share the same driver: a large remaining build (ρ = +0.67 with the jobs advantage) and a small incumbent fleet (ρ = −0.50 with fossil, −0.62 with nuclear). Where both hold, the advantage is largest."],
  {t:"REGIONS",c:GOLD},
  "Africa's mortality goes the other way at both ambition levels but neither cell is significant, and under 4% of its mortality variance is within-model — it should be reported as untestable rather than as a finding.");

bullets("Regions — the incumbent economies","Europe, China+, North America: most to retire",
  ["EUROPE is the hard case that still wins. Joint-smallest remaining build, and it loses on three fronts at once — fossil, nuclear AND bioenergy — yet it takes every family at both ambition levels: jobs +161%, deprivation +45%. It is the strongest evidence that the result is not simply 'whoever builds most wins'.",
   "Its jobs win is also the most displacement-heavy in the study: δ on fossil job loss is +0.67 to +0.80, meaning High-RE sheds substantially more fossil employment. Europe is where the transition is genuinely a substitution rather than an addition, and the just-transition framing applies most directly.",
   "CHINA+ carries the second-largest incumbent energy workforce anywhere and still sweeps, because the remaining build is larger still. It is the only region to win all three families at 2°C, and its deprivation advantage (+28%) holds despite a high starting level of energy access.",
   "NORTH AMERICA wins jobs (+117%) and deprivation (+21%) but is the ONLY region where mortality reverses significantly at BOTH ambition levels. It also holds the highest incumbent nuclear capacity of any region, and every model that contains both arms puts High-RE lower on mortality — the pooled figure disagrees with all of them, which is the Simpson signature.",
   "Together these three show the mechanism's boundary: the advantage narrows as the incumbent fleet grows, but across this database it does not reverse on jobs or deprivation in any of them."],
  {t:"REGIONS",c:GOLD},
  "Europe and China+ are also the two regions where the global label holds most tightly (δ 0.72–0.87 on their own renewable deployment), so their results carry the least classification risk.");

bullets("Regions — where the mechanism runs out","Reforming economies, Middle East, Latin America",
  ["REFORMING ECONOMIES is the just-transition case arrived at from the other direction. It carries 2.9 incumbent energy job-years per 1,000 — roughly four times any other region — and posts the weakest jobs advantage (+127%, and only +0.28 at 1.5°C, not significant).",
   "More importantly its jobs win is mostly fossil job DESTRUCTION, not renewable job creation: δ on renewable jobs is only +0.07 at 1.5°C against +0.70 on fossil job loss. That cell should be flagged in the paper rather than counted as an employment gain — the arithmetic is positive, the welfare reading is not.",
   "MIDDLE EAST is the purest fossil-CCS region at 77%, so High-CMT there means keeping the fossil fleet running — which delivers energy as well as emissions. It wins jobs comfortably (+292%) but LOSES deprivation at 2°C (δ −0.39): carbon management supplies more decent-living energy precisely because it keeps burning.",
   "LATIN AMERICA's High-CMT is 73% land-based CDR. Afforestation creates no energy jobs at all, so the jobs contrast there is close to definitional and should be presented with that caveat rather than as an independent confirmation.",
   "PACIFIC OECD is excluded from the regional results entirely: High-RE builds no more renewables there than High-CMT (δ −0.19 and −0.07), so the contrast the paper claims to measure does not exist in that region. It remains inside the World aggregate, which is a ten-region sum."],
  {t:"REGIONS",c:GOLD},
  "Four of the five deprivation reversals in the study sit in this group. They are real and regional, not artefacts, and explaining them is more credible than a clean sweep would have been.");

img("Regions","Which technologies the work sits in",
  "P4b_fuel_region.png",
  "Solar PV is the largest positive contributor in nine of eleven rows. What it displaces differs by region.",
  "Coal in the Reforming economies (−2.03, the deepest anywhere), China+ and Europe. Nuclear in North America. Gas in Latin America. The regional spread in the headline number is almost entirely a spread in what the incumbent fleet is, and how much of it there is left to retire.",
  {t:"REGIONS",c:GOLD}, 0.48);

// ============================== CLOSE ========================================
bullets("What to take away","Five things",
  ["High-renewable pathways deliver better wellbeing in 48 of 60 comparisons (80%), across nine regions plus World and both ambition levels. 38 clear a cluster-robust interval.",
   "ENERGY JOBS is the spine and it is finished: 20 of 20 cells in every sample tested, 19 significant, none against, and identical under SCI vetting. It is a construction and manufacturing dividend, largest where there is most left to build — and in India+, Rest of Asia and the Middle East it arrives with no fossil job loss at all.",
   "ENERGY DEPRIVATION follows in 16 of 20, closing the gap by a median 30% at World. It weakens under vetting but does not reverse. Its one limit: no model family holds enough of both arms to verify it within a model.",
   "AIR-QUALITY MORTALITY is held back pending one harmonised re-run. The models populating the two arms do not report ammonia on the same basis, and the difference flatters High-RE by roughly 9%.",
   "The exceptions are informative, not embarrassing: Rest of Asia trades jobs against energy access, the Reforming economies win jobs through fossil destruction rather than renewable creation, and Pacific OECD cannot be scored at all."],
  {t:"SUMMARY",c:GOLD},
  "Open: the harmonised no-ammonia mortality run · imputing NH3 for REMIND rather than deleting it everywhere (next paper) · Bergero / State of CDR scenarios, parked deliberately — adding a CDR-focused set moves the tercile thresholds and reclassifies scenarios unrelated to it.");

p.writeFile({fileName:"COMPASS_Paper1_Final.pptx"}).then(()=>console.log("slides:",N));

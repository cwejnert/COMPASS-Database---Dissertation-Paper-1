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

// ============================== JOBS =========================================
table("Method — outcome 1 of 3","Energy jobs: what is counted, where, and when",
  ["Element","Choice","Detail"],
  [[{t:"THE UNIT",o:{bold:true,color:GREEN}},{t:"JOB-YEARS, not jobs",o:{bold:true,color:GREEN}},
    "One job-year = one person employed full-time for one year. A job lasting 30 years is 30 job-years; 30 jobs lasting one year is also 30 job-years. This is a STOCK OF WORK over the whole period, NOT a headcount at any moment — so 687 job-years per 1,000 people over 2020–2100 means roughly 8.6 people in work per 1,000 at any given time, not 687"],
   [{t:"How it is computed",o:{bold:true}},"annual jobs × years, summed 2020–2100",
    "The model gives capacity by technology at each reporting node. Jobs are computed at each node, then integrated across the period. Decadal nodes are treated as representing their decade"],
   [{t:"CONSTRUCTION",o:{bold:true,color:TEAL}},"per GW BUILT in that period · transient",
    "Scales with NEW capacity added, so it appears only while building happens and disappears when it stops. Located where the plant is sited — this work cannot be imported"],
   [{t:"MANUFACTURING",o:{bold:true,color:TEAL}},"per GW BUILT in that period · transient",
    "Also scales with new capacity, but is NOT necessarily located where the plant is. Assigned to the deploying region here, which is the standard AR6 convention and an optimistic one for regions that import turbines and panels"],
   [{t:"OPERATION & MAINTENANCE",o:{bold:true,color:TEAL}},"per GW of STOCK in place · persistent",
    "Scales with installed capacity, not with additions, so it accumulates as the fleet grows and persists for the asset's life. This is the only stream that survives the end of the build-out"],
   [{t:"Fossil side",o:{bold:true}},"extraction · refining · fossil O&M","Subtracted, so the measure is NET. A pathway cannot score by building alone, and one that only shuts down coal scores zero on the renewable term"],
   [{t:"The measure",o:{bold:true}},{t:"renewable job-years − fossil job-years",o:{bold:true}},"Per 1,000 people on a FIXED base-period population, identical in every scenario, so demography cannot move a contrast"],
   [{t:"Grouping",o:{bold:true}},"renewables = wind · solar · hydro · geothermal","Nuclear and bioenergy are tracked separately and BOTH favour High-CMT (−17 and −44 job-years per 1,000 at World 1.5°C). Biomass is excluded from the renewable axis because it is the substrate of BECCS — counting it would let one scenario score on both classification axes"]],
  [2.3,3.0,6.8],{t:"JOBS",c:TEAL},
  "SPATIAL RESOLUTION: jobs are computed per R10 region from that region's own capacity, then summed to World. TEMPORAL: the three streams behave differently over time — construction and manufacturing track the RATE of building and fade, O&M tracks the STOCK and persists. That distinction is what makes the advantage front-loaded and is the mechanism behind the regional ordering.",9.5);

// ============================== DEPRIVATION ==================================
table("Method — outcome 2 of 3","Energy deprivation: how the decent-living gap is built",
  ["Element","Choice","Detail"],
  [[{t:"THE IDEA",o:{bold:true,color:GREEN}},{t:"how far BELOW a decent-living floor",o:{bold:true,color:GREEN}},
    "Not total energy use, and not inequality. A threshold is set for the final energy a person needs for decent living; the gap measures the SHORTFALL beneath it. A region above its floor contributes zero"],
   [{t:"Threshold",o:{bold:true}},"Kikstra et al. 2021, fig 1A · regional",
    "Final energy in GJ/cap/yr needed for decent living: India+ 10, China+ 15, Africa 17, Europe 28, North America 37. Regional because the same service costs different amounts of energy in different climates and settlement patterns"],
   [{t:"Sector split",o:{bold:true}},"residential/commercial · transport · industry",
    "DESIRE shares 6.7 / 11.8 / 3.8 GJ/cap, normalised to 30% / 53% / 17%. The threshold is applied SEPARATELY IN EACH SECTOR, not to the total"],
   [{t:"Efficiency path",o:{bold:true}},"−1.9%/yr on the threshold, floored at 50%",
    "The energy needed to deliver decent living falls over time as service provisioning improves, so the bar drops. Floored at half its 2020 value so it cannot collapse to nothing by 2100"],
   [{t:"THE GAP — step by step",o:{bold:true,color:GOLD}},{t:"gap = max(0, threshold − actual)",o:{bold:true,color:GOLD}},
    "(1) For each sector, subtract that sector's delivered final energy per capita from that sector's threshold. (2) TRUNCATE AT ZERO: if delivered exceeds the threshold the sector contributes 0, never a negative. (3) Sum the three sectors. (4) Sum over years 2020–2100. The result is cumulative GJ per capita of shortfall"],
   [{t:"Why truncation matters",o:{bold:true,color:RED}},{t:"surplus never offsets shortfall",o:{color:RED}},
    "Because negatives are clipped, only sectors where a region falls SHORT can move the measure. Abundant industrial energy cannot compensate for a residential shortfall, and a rich region cannot offset a poor one. This makes it a DEPRIVATION measure rather than a net-adequacy one"],
   [{t:"Second measure",o:{bold:true}},"headcount below threshold (%)","Share of population living below the floor. Moves with the gap in every cell; reported as a within-family check and because it converts directly into people"]],
  [2.3,3.0,6.8],{t:"DEPRIVATION",c:TEAL},
  "WHAT IT CANNOT SEE: the threshold is applied to a REGIONAL AVERAGE, so the measure is a regional aggregate and cannot say who inside a region is deprived. The implied within-region inequality is lower than observed, so it understates deprivation in unequal regions.",9.5);

// ============================== MORTALITY ====================================
table("Method — outcome 3 of 3","PM2.5 mortality: from emissions to deaths, and the input rule",
  ["Element","Choice","Detail"],
  [[{t:"THE CHAIN",o:{bold:true,color:GREEN}},{t:"emissions → concentration → exposure → deaths",o:{bold:true,color:GREEN}},
    "(1) A scenario reports precursor emissions by region and year. (2) TM5-FASST converts them to ambient PM2.5 concentrations. (3) Concentrations are combined with population to give exposure. (4) An exposure–response function converts exposure to premature deaths"],
   [{t:"Atmospheric model",o:{bold:true}},"TM5-FASST via the rfasst package",
    "A reduced-form SOURCE–RECEPTOR model: pre-computed coefficients say how much a tonne emitted in region A raises concentration in region B. This captures cross-boundary transport without running a full chemistry-transport model per scenario"],
   [{t:"Precursors",o:{bold:true}},"SO₂ · NOₓ · black carbon · organic matter · NH₃",
    "BC and OM are primary particles emitted directly. SO₂, NOₓ and NH₃ are gases that form SECONDARY particles in the atmosphere — ammonium sulfate and ammonium nitrate — which is why ammonia matters despite not being a particle itself"],
   [{t:"THE INPUT RULE",o:{bold:true,color:GREEN}},{t:"all five reported DIRECTLY at R10",o:{bold:true,color:GREEN}},
    "A scenario qualifies only if it reports every precursor at regional level. No World-to-R10 population disaggregation, no ammonia filled in from a sidecar, no gap-filling. 422 of 643 classified targets qualify; 150 are REJECTED rather than filled (98 report no precursor, 52 have incomplete grids) and 71 have no direct R10 input at all"],
   [{t:"Why the rule exists",o:{bold:true,color:RED}},{t:"models file emissions differently",o:{color:RED}},
    "Ammonia is 6–12% of PM2.5 mortality in IMAGE, POLES-JRC, AIM and MESSAGEix and 0.15% in REMIND, because REMIND's agricultural emissions live in MAgPIE and never reach Emissions|NH3. Filling that gap would compare accounting conventions; requiring direct reporting removes the asymmetry at source"],
   [{t:"Integration",o:{bold:true}},"trapezoidal across decadal nodes, 2020–2100",
    "Annual deaths at each node, area under the curve between nodes, summed. Reported as MILLION CUMULATIVE DEATHS over the century — an absolute count, not a rate, and not per capita"],
   [{t:"The cost",o:{bold:true}},"smaller arms than the other outcomes","42 vs 37 scenarios at World 1.5°C, against 42 vs 43 for jobs. Mortality is always the binding constraint on sample size, which is why its intervals are the widest"]],
  [2.3,3.0,6.8],{t:"MORTALITY",c:TEAL},
  "SPATIAL: concentrations are computed on TM5-FASST's own 56-region grid and aggregated to R10, so cross-border transport between R10 regions is captured. TEMPORAL: emissions at decadal nodes drive concentrations at those nodes; between-node behaviour is interpolated, and no run in this release required interpolation.",9.5);

p.writeFile({fileName:"COMPASS_methodology_slides.pptx"}).then(()=>console.log("methodology slides:",N));

//NoMAD
//Noise and People.
//Created by Joseph Wil <joe@josephwil,net>
//
// Hex parts based on nigo quilez work - iq/2014
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iBeatCount;
uniform float iBeatTotalCount;
uniform float iMeasureCount;

#define RANDOM_LETTERS 1
#define TOTAL_BEATS 128.0
#define STATIC_LETTERS 0
#define SHOW_GLOW 1
#define PANIC 0

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float rand2(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float averageForRadius(vec2 co, float radius) {
  float average = float(0);
  float sampleSize = 5.0;
  float y2 = 0.0;
  float x2;
  float x3;

  x2 = sqrt(pow(radius, 10.0) - pow(y2, 2.0));
  x3 = (x2 - sampleSize) * -2.0;
  y2 += sampleSize;
  return average;
}

vec4 buildNoise(float direction)
{
  float pos;
  if(direction == -1.0){
    pos = 1.;
  }
  else{
    pos = 0.;
  }

  float noiseGrowth = 0.;
  float speed = 0.01;
  vec2 position = pos-gl_FragCoord.xy / iResolution.xy;
  position = position + vec2(1/iResolution.x, iResolution.y);
  vec4 colour = vec4(0.,0.,0.,0.);
  float random = rand(vec2(position.x/direction + iGlobalTime*speed, position.y));
  random = random - noiseGrowth;
  float randomMultiplier =position.x/direction * 10.1;
  colour += random * randomMultiplier;
  float multiplier = (position.x / 0.05);

  if(direction == -1.0){
    colour -= multiplier;
  }
  else{
    colour += multiplier;
  }
  return mix(colour, vec4(0.,0.,0.,0.), 0.5);
}

vec4 hexagon( vec2 p )
{
  int edges = 3;
  float edgeWidth = 1.0;//0.4;
  vec2 q = vec2( p.x*2.0*0.5773503, p.y + p.x*0.5773503 );

  vec2 pi = floor(q);
  vec2 pf = fract(q);

  float v = mod(pi.x + pi.y, edges);

  float ca = step(1.0,v);
  float cb = step(2.0,v);
  vec2  ma = step(pf.xy,pf.yx);

  // Borders
  float e = dot( ma, edgeWidth-pf.yx + ca*(pf.x+pf.y-1.0) + cb*(pf.yx-2.0*pf.xy) );

  // Center circle dots
  float f = length( (fract(p) - 0.5)*vec2(1.0,0.85) );

  return vec4( pi + ca - cb*ma, e, f );
}

float hash1( vec2  p ) { float n = dot(p,vec2(127.1,311.7) ); return fract(sin(n)*43758.5453); }

float noise( in vec3 x )
{
  vec3 p = floor(x);
  vec3 f = fract(x);
  f = f*f*(3.0-2.0*f);
  vec2 uv = (p.xy+vec2(37.0,17.0)*p.z) + f.xy;
  vec2 rg = texture2D( iChannel2, (uv+0.5)/256.0, -100.0 ).yx;
  return mix( rg.x, rg.y, f.z );
}

vec4 populationDensity(vec2 pos)
{
  float person = 0.0;
  float personGirth = 4.0;
  float maximumCapacity = 0.5;
  float walkingSpeed = 0.00000008;
  float populationSize = 0.01;
  float urgencyRate = 0.002;

  //walkingSpeed *=  sin(iBeat+0.5);
  //  incrementSize =+ iOvertoneVolume*0.01;
  float direction = 1.0;

  if(iMeasureCount > 3.0){
    direction = -1.0;
  }
  else{
    direction = 1.0;
  }

  //    if(iBeat == 1){
    for (float i=0.0; i<maximumCapacity; i+=populationSize) {
      float seed = iGlobalTime*walkingSpeed+i;
      //      vec2 point = vec2(rand2(vec2(sin(iGlobalTime*0.0000001), seed)), rand2(vec2(seed, seed)));
      vec2 point = vec2(rand2(vec2(direction*seed, direction*0.5)), rand2(vec2(direction*0.5, direction*seed)));

      if (abs(sqrt(pow(pos.x-point.x,personGirth)+pow(pos.y-point.y-0.1,personGirth))/1.0) < 0.0001) {
        person += (urgencyRate/i) * (iBeat + iMeasureCount * 0.09);
      }
    }
    //    }

    return vec4(vec3(iMeasureCount*0.01+person, iBeatCount*0.01+person, iOvertoneVolume*0.01+person),1.0);
}

vec4 hex(vec2 uv){
  vec2 pos = (-iResolution.xy + 2.0*gl_FragCoord.xy)/iResolution.y;

  float scale = 10.0;
  float speed = sin(iGlobalTime * 0);

  // gray
  vec4 h = hexagon(scale*pos + 0.01);
  float n = noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) );
  vec3 col = 0.15 + 0.15*hash1(h.xy+1.2)*vec3(1.0);
  col *= smoothstep( 0.10, 0.11, h.z );
  col *= smoothstep( 0.10, 0.11, h.w );
  col *= 1.0 + 0.15*sin(40.0*h.z);
  col *= 0.75 + 0.5*h.z*n;

  // red
  h = hexagon(scale*pos + 0.01);
  n = noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) );
  vec3 colb = 0.9 + 0.8*sin( hash1(h.xy)*1.5 + 2.0 + vec3(0.0,1.0,1.0) );
  colb *= smoothstep( 0.10, 0.11, h.z );
  colb *= 1.0 + 0.15*sin(40.0*h.z);
  colb *= 0.75 + 0.5*h.z*n;

  h = hexagon(scale*(pos+0.1*vec2(-1.3,1.0)) + 0.5);
  col *= 1.0-0.8*smoothstep(0.45,0.451,noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) ));

  col = mix( col, colb, smoothstep(0.45,0.451,n) );
  col *= pow( 16.0*uv.x*(1.0-uv.x)*uv.y*(1.0-uv.y), 0.1 );
  return vec4( col, 1.0 );
}

vec3 hsvToRgb(float mixRate, float colorStrength){
  float colorChangeRate = 18.0;
  float time = fract(iGlobalTime/colorChangeRate);
  float movementStart = (iBeatCount == 0) ? 1.0 : 0.5;
  vec3 x = abs(fract((iBeatCount-1+time) + vec3(2.,3.,1.)/3.) * 6.-3.) - 1.;
  vec3 c = clamp(x, 0.,1.);
  //c = c*iBeat;
  //c = c * clamp(iBeat, 0.1, 0.4)+0.6;
  return mix(vec3(1.0), c, mixRate) * colorStrength;
}

vec4 addGlow(vec2 uv, vec2 v, float glow)
{
  vec4 glowing = vec4(0.0);

  if(iBeat == 1.0){
    glow += 0.0005;
  }

  if(iOvertoneVolume < 0.01){
    glow = -1.0;
    glowing = vec4(-1.);
  }
  else{
    //    glow =+ iOvertoneVolume * 0.005;

    float res = glow / length(v - uv);
    glowing = res * vec4(hsvToRgb(0.5, 0.9),1.0);
  }

  return glowing;
}

vec4 lineDistort(vec4 cTextureScreen, vec2 uv1){
  float sCount = 900.;
  float nIntensity=0.1;
  float sIntensity=0.2;
  // sample the source
  float x = uv1.x * uv1.y * iGlobalTime *  1000.0;
  x = mod( x, 13.0 ) * mod( x, 123.0 );
  float dx = mod( x, 0.05 );
  vec3 cResult = cTextureScreen.rgb + cTextureScreen.rgb * clamp( 0.1 + dx * 100.0, 0.0, 1.0 );
  // get us a sine and cosine
  vec2 sc = vec2( sin( uv1.y * sCount ), cos( uv1.y * sCount ) );
  // add scanlines
  cResult += cTextureScreen.rgb * vec3( sc.x, sc.y, sc.x ) * sIntensity;
  // interpolate between source and result by intensity
  cResult = cTextureScreen.rgb + clamp(nIntensity, 0.0,1.0 ) * (cResult - cTextureScreen.rgb);

  return vec4(cResult, cTextureScreen.a);
}

vec4 buildCell(vec2 uv, vec2 point, int still){
  float person = 1.0;
  float inc;
  float movementScale = .0000001;

  if(still==0){

  if(iBeatTotalCount >= 64.0){
    //point.y = rand2(vec2(point.x,point.y*iGlobalTime*movementScale));
  }
  else{
    //point.y = 1-rand2(vec2(point.y,point.x*iGlobalTime*movementScale));
  }

  if(STATIC_LETTERS == 1){
    float rate = 0.3;
    //point.x = 0.5+0.5*sin(point.x*iGlobalTime*rate);
    //point.y = 0.5+0.5*sin(point.y+iGlobalTime*rate);

    float invBeatTotal = TOTAL_BEATS-iBeatTotalCount;

      if(iBeatTotalCount > 64.0){
        point.y -= 0.2+0.5*sin(iBeatTotalCount*0.009/point.x)*1/invBeatTotal-0.2;
      }
      else{
        point.y -= (0.2+0.5*sin(invBeatTotal*0.009/point.x))*1/iBeatTotalCount;
      }

  }else{

    if(iBeatTotalCount > 64.0){
      point.y -= 0.2+0.5*sin(iBeatTotalCount*0.04/point.x);
    }
    else{
      point.y -= 0.2+0.5*sin((TOTAL_BEATS-iBeatTotalCount)*0.04/point.x);
    }
  }

  //point.x += sin(iBeatTotalCount*0.1)*0.5;
  }
  float cell = abs(sqrt(pow(uv.x-point.x,4.0)+pow(uv.y-point.y, 4.0)));

  if (cell > 0.0001){
    person -= 1.0;
  }else if (cell < 0.0001){
    person -= 0.9;
  }
  vec4 helloPoint = vec4(vec3(person),1.0);

  if(SHOW_GLOW==1){
    helloPoint += addGlow(uv, point, 0.003);
  }

  return helloPoint;
}

vec4 letter(mat3 letter, vec2 offset, vec2 uv){
  vec2 point = vec2(0,0);
  vec4 helloPoint = vec4(0,0,0,0);
  vec3 xPos = vec3(0., 0.03, 0.06);
  vec3 yPos = vec3(0.06, 0.03, 0);
  float letterSpace = 0.1;

  for(int y=0; y < 3; y++){
    for(int x=0; x < 3; x++){
      if(letter[y][x] == 1){
        point = vec2(xPos[x]+offset.x, offset.y+yPos[y]);
        helloPoint += buildCell(uv, point, 0);
      }
    }
  }
  return helloPoint;
}

vec4 bouncingPerson(vec2 uv){
  float letterSpace = 0.06;
  vec4 helloPoint = vec4(0.0);

  mat3 complete = mat3(1, 1, 1,  1, 1, 1,  1, 1, 1);
  mat3 letterR  = mat3(1, 1, 1,  1, 1, 0,  1, 0, 1);
  mat3 letterE  = mat3(1, 1, 1,  1, 1, 0,  1, 1, 1);
  mat3 letterP  = mat3(1, 1, 1,  1, 1, 1,  1, 0, 0);
  mat3 letterL  = mat3(1, 0, 0,  1, 0, 0,  1, 1, 1);

  mat3 letterC  = mat3(1, 1, 1,  1, 0, 0,  1, 1, 1);
  mat3 letterT  = mat3(1, 1, 1,  0, 1, 0,  0, 1, 0);
  mat3 letterI  = mat3(0, 1, 0,  0, 1, 0,  0, 1, 0);


  helloPoint += letter(letterR, vec2(0.3+letterSpace*0, 0.45), uv);

  if(iOvertoneVolume > 0.1){
    helloPoint += letter(letterE, vec2(0.3+letterSpace*2, 0.45), uv);

    if(PANIC == 0){
    helloPoint += letter(letterP, vec2(0.3+letterSpace*4, 0.45), uv);
     helloPoint += letter(letterL, vec2(0.3+letterSpace*6, 0.45), uv);
    }
  }

  if(RANDOM_LETTERS == 0){
    float liveUntil =  1/iGlobalTime*4;
    //Save processing if we have already faded out
    if(liveUntil > 0.1){
      mat3 invertedR = complete - letterR;
      mat3 invertedE = complete - letterE;
      mat3 invertedP = complete - letterP;
      mat3 invertedL = complete - letterL;

      helloPoint += letter(invertedR, vec2(0.3+letterSpace*0, 0.45), uv) * liveUntil;
      helloPoint += letter(invertedE, vec2(0.3+letterSpace*2, 0.45), uv) * liveUntil;
      helloPoint += letter(invertedP, vec2(0.3+letterSpace*4, 0.45), uv) * liveUntil;
      helloPoint += letter(invertedL, vec2(0.3+letterSpace*6, 0.45), uv) * liveUntil;
    }
    else{
      helloPoint += letter(letterE, vec2(0.05+letterSpace*0, 0.30), uv);
      helloPoint += letter(letterL, vec2(0.05+letterSpace*2, 0.3), uv);
      helloPoint += letter(letterE, vec2(0.05+letterSpace*4, 0.3), uv);
      helloPoint += letter(letterC, vec2(0.05+letterSpace*6, 0.3), uv);
      helloPoint += letter(letterT, vec2(0.05+letterSpace*8, 0.3), uv);
      helloPoint += letter(letterR, vec2(0.05+letterSpace*10, 0.3), uv);
      helloPoint += letter(letterI, vec2(0.05+letterSpace*12, 0.3), uv);
      helloPoint += letter(letterC, vec2(0.05+letterSpace*14, 0.3), uv);
    }
  }

  return helloPoint;
}

vec4 theCell(vec2 uv){
  vec4 x  = vec4(0.,0.,0.,0.);
  float t = 0.0;

  int rowCount = 4;
  int cellRate = 1;
  for(int i = 0; i < iBeatTotalCount/4; i+= 4){
    t = i/rowCount;
    vec2 thing = vec2(rand(vec2(0.45+0.03 * mod(i, rowCount), 0.03*t+0.05)), rand(vec2(0.4, i)));
    x += buildCell(uv, thing , 0);
  }

  return x;
}


vec3 mod289(vec3 x){
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x){
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v){
  const vec4 C = vec4(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439);
  vec2 i = floor(v + dot(v, C.yy));
  vec2 x0 = v - i + dot(i, C.xx);

  vec2 i1;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0 )) + i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m;
  m = m*m;

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

  m *= 1.79284291400159 - 0.85373472095314 * (a0*a0 + h*h);

  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

vec2 warpedCords(vec2 p){
  float distortion=0.0001;
  float distortion2=0.5;
  float speed=0.05;
  float rollSpeed=0.0;
  float ty = iGlobalTime* speed;
  float yt = p.y - ty;
  float offset = snoise(vec2(yt*3.0,0.0))*0.2;
  offset = pow(offset*distortion, 3.0)/distortion ;
  offset += snoise(vec2(yt*50.0,0.0))*distortion2*0.001;

  vec2 o = vec2(fract(p.x + offset),-fract(p.y-iGlobalTime*rollSpeed));
  return o;
}


vec4 generateSnow(vec2 p, vec4 tex){
  float size = 2.;
  vec4 color = tex;
  float amount=0.3;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  vec4 snow = vec4(rand(vec2(xs,ys*iGlobalTime/1800))*amount);
  return snow;
}



void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;



  float noiseWeight = 0.0;
  float hexWeight   = 0.0;
  float populationWeight = 0.0;
  float spellWeight = 0.0;

  vec4 leftNoise  = vec4(0.);
  vec4 rightNoise = vec4(0.);

  if(noiseWeight == 1.0){
    leftNoise  = buildNoise(1);
    rightNoise = buildNoise(-1);
  }

  vec4 spelling = vec4(0.);
  if(spellWeight == 1.0){
    spelling = bouncingPerson(uv);
  }

  vec4 hexWorld = vec4(0.);
  if(hexWeight == 1.0){
    hexWorld = hex(uv);
  }

  vec4 population = vec4(0.);
  if(populationWeight == 1.0){
    population = populationDensity(uv);
  }



  gl_FragColor =  theCell(uv) + lineDistort((spelling * spellWeight) +
    ((1-(leftNoise * rightNoise)) * noiseWeight) +
    hexWorld * hexWeight + population * populationWeight, uv);
}

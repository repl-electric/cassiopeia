//NYC by Joseph Wilk <joe@josephwilk.net>
uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iGlobalBeatCount;

uniform float iCircleCount;
uniform float iColor;
uniform float iHalfPi;
uniform float iInOutSpeed;
uniform float iInvertColor;
uniform float iSnowRatio;
uniform float iDeformCircles;
uniform float iDeath;

uniform float iCircularWeight;
uniform float iPopulationWeight;
uniform float iBouncingWeight;
uniform float iNycWeight;
uniform float iCircleDanceWeight;
uniform float iCircleDanceColor;
uniform float iSplatter;
uniform float iCircleDistort;

const float pi = 3.14159265;
const mat2 m = mat2(0.80,  0.60, -0.60,  0.80);

#define CIRCLE_ACCELERATOR 0.00000001
#define CIRCLE_SCALE 1.25
#define CELL_DANCE 2

#define TOTAL_BEATS 128.0
#define STATIC_LETTERS 0
#define SHOW_GLOW 1

float measureCount = mod(iGlobalBeatCount, 8 * 16) / 8;
float beatTotalCount = mod(iGlobalBeatCount, 16);

vec3 hsv2rgb(float h, float s, float v) {
  return mix(vec3(1.), clamp((abs(fract(h+vec3(3.,2.,1.)/3.)*6.-3.)-1.),0.,1.),s)*v;
}

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float rand2(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

vec3 hash3( vec2 p ){
  vec3 q = vec3(dot(p,vec2(127.1,311.7)),
              dot(p,vec2(269.5,183.3)),
              dot(p,vec2(419.2,371.9)) );
  return fract(sin(q)*43758.5453);
}

float noise(float x, float y){return sin(1.5*x)*sin(1.5*y);}

mat2 mm2(in float a){
  float c = abs(cos(a));
  float s = sin(a);
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  float FREQ_SCALE = (4096.0/4096.0);
  float AMP_SCALE = 0.5;
  float fi = FREQ_SCALE*uv.y;
  float fid  = FREQ_SCALE/4096.0/2.0;
  float adjust = AMP_SCALE * 0.5 *   (max(0.0, texture2D(iChannel0, vec2(fi,0.25)).x) +
                                      max(0.0, texture2D(iChannel0, vec2(fi+fid,0.25)).x));

  if(iOvertoneVolume == 0.0){
    adjust = 1;
  }

  return mat2(c * adjust, -s * adjust,s * adjust ,c * adjust);
}

float saturate(float a){ return clamp( a, 0.0, 1.0 );}
// Fractional Brownian Motion code by IQ.
// http://en.wikipedia.org/wiki/Fractional_Brownian_motion

float fbm4( float x, float y ){
  vec2 p = vec2( x, y ) * iCircleDistort;
  float f = 0.0;

  if(iBeat == 1.0){
    f = .02;
  }

  f +=  0.5000*noise(p.x, p.y);
  p = m*p*2.02 + measureCount * 0.0009;
  f +=  0.2500*noise(p.x, p.y);
  p = m*p*2.03 + measureCount * 0.0009;
  f +=  0.1250*noise(p.x, p.y);
  p = m*p*2.01 + measureCount * 0.0009;

  float fudge = 0.0625;
  if(iCircleDistort == -0.1){
    fudge = 0.0725;
  }

  f +=  fudge*noise(p.x, p.y);

  return f / (0.9375 + (measureCount * 0.009 * 4));
}

const float linesmooth = 0.0333;
const float tau = 6.28318530717958647692;

vec4 circleDance(void){
  vec2 uv = (gl_FragCoord.xy - iResolution.xy*.5)/iResolution.x;

  float seperation = 0.4;
  vec3 wave = vec3(0.0);
  float colorOffset;

  float n = min(60.0, iCircleDanceWeight);
  float width = 4.0/500;
  uv = vec2(abs(atan(uv.x,uv.y)/(.5*tau)),length(uv));

  if(iCircleDanceWeight==1.0){
    width = min(4.0/500,4.0/iSplatter);
    uv.x *= 1.0/10.0;
  }
  else{
    uv.x *= 1.0/80.0;
  }

  colorOffset = iCircleDanceColor;
  for (int i=0; i < n; i++){
    float sound = texture2D(iChannel0, vec2(uv.x,.75)).x;
    float a = 0.1*float(i)*tau/float(n) + colorOffset;
    vec3 phase = smoothstep(-1.0,.5,vec3(cos(a),cos(a-tau/3.0),cos(a-tau*2.0/3.0)));
    wave += phase*smoothstep(width, 0.0, abs(uv.y - ((sound*0.9)+0.2)));
    uv.x += seperation/float(n);
  }

  wave *= 10.0/float(n);
  return vec4(wave,1);
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
  //incrementSize =+ iOvertoneVolume*0.01;
  float direction = 1.0;

  if(measureCount > 3.0){
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
      person += (urgencyRate/i) * (iBeat + measureCount * 0.09);
    }
  }
  //    }

  return vec4(vec3(measureCount*0.01+person, mod(iGlobalBeatCount,16)*0.01+person, iOvertoneVolume*0.01+person),1.0);
}

vec4 generateSnow(vec2 p, float speed){
  float size = 2.;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  //vec4 snow = vec4(rand(vec2(xs*iGlobalBeatCount*0.0000008, smoothstep(0.01, 0.02, iGlobalBeatCount))));
  //  vec4 snow = vec4(rand(vec2(xs/iGlobalBeatCount*202, smoothstep(0.01, 0.02, iGlobalBeatCount))));
  vec4 snow = vec4(rand(vec2(xs, ys * iGlobalBeatCount * speed)));
  return snow;
}

vec4 circular(void){
  vec2 mainuv = (gl_FragCoord.xy / iResolution.xy);
  float aspect = iResolution.x/iResolution.y;
  float finalval = iColor; // 0.1
  float scale = CIRCLE_SCALE;
  float speed = CIRCLE_ACCELERATOR;
  float shading = 0.20025;
  float halfpi = iHalfPi;
  float circleScale = min(60, iCircleCount);

  float speedFactor= 0.07;
  if(iDeath < 3.0){
    speedFactor = iDeath+8.0 * speedFactor;
    scale = iDeath*0.325 * CIRCLE_SCALE;
    //    halfpi= 2.0;
    ///scale = iOvertoneVolume+10.1*10.0;
    //    circleScale = circleScale * iDeath;
  }
  if(iOvertoneVolume < 0.001){
    //halfpi = 0.01;
  }

  vec2 uv = mainuv * scale - scale * 0.5;
  uv.x *= aspect;

  float mainval = 1.0; //50-60 nice overpaint effect
  float inverseLength;

  //  if(iDeformCircles == 1.0){

  float beatBump = 0.002;
  if(iNycWeight == 0.02){
    beatBump = 0.004;
  }

  float fuzzCircle = iNycWeight * rand2(vec2(uv.x*iGlobalBeatCount, uv.y+iBeat)) + 0.008*iBeat;
  inverseLength = saturate(length(uv)) + clamp(iBeat, 0.0, beatBump) - fuzzCircle;
    //  }
    //  else{
  //        inverseLength = saturate(length(uv)) * (uv.x + uv.y) * iDeformCircles;
    //  }

  //inverseLength = saturate(length(uv)) * rand(vec2(texture2D(iChannel0, vec2(0, 0)).x, texture2D(iChannel0, vec2(0, 0)).y));

  float core = inverseLength * circleScale;
  float coreident = ceil(core);

  speed = 1e-4 + clamp(iBeat,0.0,0.0000001);
  shading = 0.20025; //clamp(iOvertoneVolume, 0.20025, 0.20025);

  //float musiz = texture2D(iChannel0, vec2(0.,0.)).x / 100.0 + iOvertoneVolume/1;
  //
  //vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.005 , iGlobalTime * 0.005) * pi * pi );
  //rotatedUVs *= mm2( halfpi - fbm4( coreident * 2.0 , iGlobalTime * 0.05) * pi * pi )  * musiz;
  //  * texture2D(iChannel0, vec2(0, 0)).x

  vec2 rotatedUVs = uv * mm2(halfpi + fbm4(coreident * 0.005 , iGlobalTime * 0.03 *
                                           clamp(texture2D(iChannel0, vec2(0., 0.75)).x, 0.4, 1.0)) * pi * pi );
  rotatedUVs *= mm2( halfpi - fbm4(coreident * 2.0 , iGlobalTime *  speedFactor *
                                   clamp(texture2D(iChannel0, vec2(0., .75)).x, 0.3, 1.0) ) * pi * pi );

  //  vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.5, iGlobalTime * 0.07 ) * pi * pi );
  //rotatedUVs          *= mm2( halfpi - fbm4( coreident * 2.0, iGlobalTime * 0.1  ) * pi * pi );
  float arcpos = ( pi + atan( rotatedUVs.y, rotatedUVs.x ) ) / halfpi;
  arcpos /= pi;
  arcpos = smoothstep( 0.2, shading - coreident * 0.0001, fract( arcpos ) * fract( -arcpos ) );


  //  vec2 rotatedUVs = uv * mm2(halfpi + fbm4(coreident * 0.005 , iGlobalBeatCount * speed * texture2D(iChannel0, vec2(0, 0)).x) * pi * pi);
  // rotatedUVs *= mm2(halfpi - fbm4(coreident * 2.0 , iGlobalBeatCount * speed * texture2D(iChannel0, vec2(0, 0)).x) * pi * pi);

  //float arcpos = (pi + atan(rotatedUVs.y, rotatedUVs.x)) / halfpi;
  //arcpos /= pi;

  //arcpos = smoothstep(0.2, shading - coreident * 0.0001, fract(arcpos) * fract(-arcpos));
  mainval *= fbm4(coreident, iGlobalBeatCount * iInOutSpeed) * arcpos;

  float coresmooth = fract(core) * fract(-core);
  float corewidth  = fwidth(coresmooth);
  float edgethreshold = 0.1; //min(iDeformCircles, 0.2101);
  mainval *= smoothstep(edgethreshold - corewidth, edgethreshold + corewidth, coresmooth);
  finalval += mainval;

  finalval = max(finalval, 0.0) + 0.0025;
  finalval = min(finalval, 1.0);
  float colorIntensity = iDeformCircles;
  finalval =  pow(finalval, 1.0/2.0) + colorIntensity;

  vec4 r = vec4(vec3(finalval) - iBeat * 0.1, 1.0);
  return r;
}

vec3 hsvToRgb(float mixRate, float colorStrength){
  float colorChangeRate = 18.0;
  float time = fract(iGlobalTime/colorChangeRate);
  float movementStart = (mod(iGlobalBeatCount,16) == 0) ? 1.0 : 0.5;
  vec3 x = abs(fract((mod(iGlobalBeatCount,16)-1+time) + vec3(2.,3.,1.)/3.) * 6.-3.) - 1.;
  vec3 c = clamp(x, 0.,1.);
  //c = c*iBeat;
  //c = c * clamp(iBeat, 0.1, 0.4)+0.6;
  return mix(vec3(1.0), c, mixRate) * colorStrength;
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

float smoothbump(float center, float width, float x){
  float w2 = width/2.0;
  float cp = center+w2;
  float cm = center-w2;
  float c = smoothstep(cm, center, x) * (1.0-smoothstep(center, cp, x));
  return c;
}

vec4 colorCycle = vec4(hsvToRgb(0.5, 0.9), 1.0);

vec4 addGlow(vec2 uv, vec2 point, float glow){
  if(iBeat == 1.0){
    glow += 0.00005;
  }
  float res = glow / length(point - uv);
  return res * colorCycle;
}

vec4 buildCell(vec2 uv, vec2 point, int still){
  float person = 1.0;
  float movementScale = .0000001;
  float speedFactor = 0.1;
  int wavey = 0;
  if(iOvertoneVolume < 0.01){
    speedFactor = speedFactor * 0.1;
    wavey = 1;
  }

  if(still==0){
    float y1 = 1.0;
    if(iBouncingWeight == 2.0 && iCircularWeight == 0.0){
      y1 = 0.5+0.5*sin(iGlobalTime*0.1);
    }
    if(mod(iGlobalTime, TOTAL_BEATS) > 64.0){
      point.y -= y1 * sin(iGlobalTime*0.1/point.x);
    }
    else{
      point.y -= y1 * sin((TOTAL_BEATS-mod(iGlobalTime, TOTAL_BEATS))*0.1/point.x);
    }
  }

  if(CELL_DANCE==1){
    //Cells become a sound wave
    float d = smoothstep(0, 1.0, texture2D(iChannel0, vec2(point.x, 0.75)).x) * 0.8;
    point.y = 0.1*(d)  + 0.5;
    point.y = 1.0-point.y;
  }
  else if(iBouncingWeight == 5.0 || wavey == 1){
    //Cells dance in a circle
    float poo = point.x;
    float p = sin(iGlobalTime*speedFactor)*0.001;
    point.x = 0.5 + p*cos(mod((iGlobalTime+mod(iGlobalTime*0.2,360)*poo),360))*0.1;
    point.y = 0.5 + p*sin(mod(iGlobalTime+mod(iGlobalTime*0.2,360)*poo,360))*0.1;
  }

  //round cells
  float p;
  float cellBoundries;
  p = 2.0;
  cellBoundries = 0.5;

  float xy = length(uv-point);
  float cell = smoothstep(xy, 0.01+xy, 1.0);

  if (cell > cellBoundries){
    person =  0.0;
  }else if (cell < cellBoundries){
    person = 0.1;
  }

  vec4 helloPoint = vec4(vec3(person), 1.0);

  if(person != 0.1){
    float glowFactor;

    if(iBouncingWeight <= 3.0){
      glowFactor = 0.0139;
    }
    else{
      if(iBouncingWeight >= 5.0){
        glowFactor = iBouncingWeight * 0.15;
      }
      else{
        glowFactor = 0.04;
      }
    }

    if(iDeath != 3.0){
      glowFactor *= (0.45*(iDeath));
    }

    if(SHOW_GLOW==1){
      helloPoint += addGlow(uv, point, glowFactor);
    }
  }
  return helloPoint;
}

vec4 letter(mat3 letter, vec2 offset, vec2 uv){
  vec2 point = vec2(0,0);
  vec4 helloPoint = vec4(0,0,0,0);
  vec3 xPos = vec3(0.01, 0.03, 0.05);
  vec3 yPos = vec3(0.05, 0.03, 0.01);

  for(int y=0; y < 3; y++){
    for(int x=0; x < 3; x++){
      if(letter[y][x] == 1){
        point = vec2(xPos[x]+offset.x, offset.y+yPos[y]);
        helloPoint += buildCell(uv, point, STATIC_LETTERS);
      }
    }
  }
  return helloPoint;
}

const mat3 LETTER_COMPLETE = mat3(1, 1, 1,  1, 1, 1,  1, 1, 1);
const mat3 LETTER_R        = mat3(1, 1, 1,  1, 1, 0,  1, 0, 1);
const mat3 LETTER_E        = mat3(1, 1, 1,  1, 1, 0,  1, 1, 1);
const mat3 LETTER_P        = mat3(1, 1, 1,  1, 1, 1,  1, 0, 0);
const mat3 LETTER_L        = mat3(1, 0, 0,  1, 0, 0,  1, 1, 1);
const mat3 LETTER_C        = mat3(1, 1, 1,  1, 0, 0,  1, 1, 1);
const mat3 LETTER_T        = mat3(1, 1, 1,  0, 1, 0,  0, 1, 0);
const mat3 LETTER_I        = mat3(0, 1, 0,  0, 1, 0,  0, 1, 0);

vec4 bouncingPerson(vec2 uv){
  float letterSpace = 0.05;
  float top = 0.40;
  float topLower = 0.2;
  float leftLower = 0.12;
  float leftTop = 0.32;
  vec4 helloPoint = vec4(0.0);
  float bounceWeight = iBouncingWeight;

  if(iCircularWeight != 0.0 && bounceWeight < 3.0){
    bounceWeight = 3.0;
  }

  //if(iOvertoneVolume > 0.01){
    if(bounceWeight >= 5.0){//single center cell
      vec2 uvt = gl_FragCoord.xy / iResolution.xy;
      helloPoint += buildCell(uvt, vec2(0.5, 0.5), 0);
    }
    else{
      helloPoint += letter(LETTER_R, vec2(leftTop+letterSpace*0, top), uv);
      helloPoint += letter(LETTER_E, vec2(leftTop+letterSpace*2, top), uv);
      helloPoint += letter(LETTER_P, vec2(leftTop+letterSpace*4, top), uv);
      helloPoint += letter(LETTER_L, vec2(leftTop+letterSpace*6, top), uv);
    }

    //}

  if(bounceWeight <= 3.0){
    float spellingConvergePoint = 0.0;
    if(iBouncingWeight == 2.0 && iCircularWeight == 0.0){
      spellingConvergePoint = 0.5+0.5*sin(iGlobalTime*0.1);
    }

    helloPoint += letter(LETTER_E, vec2(leftLower+letterSpace*0, topLower), uv);
    helloPoint += letter(LETTER_T, vec2(leftLower+letterSpace*8, topLower), uv);
    if(spellingConvergePoint < 0.3){
      helloPoint += letter(LETTER_C, vec2(leftLower+letterSpace*6, topLower), uv);
      helloPoint += letter(LETTER_E, vec2(leftLower+letterSpace*4, topLower), uv);
    }
    helloPoint += letter(LETTER_L, vec2(leftLower+letterSpace*2, topLower), uv);
    helloPoint += letter(LETTER_I, vec2(leftLower+letterSpace*12, topLower), uv);

    helloPoint += letter(LETTER_R, vec2(leftLower+letterSpace*10, topLower), uv);
    helloPoint += letter(LETTER_C, vec2(leftLower+letterSpace*14, topLower), uv);
  }
  return helloPoint;
}

vec4 theCell(vec2 uv){
  vec4 x  = vec4(0.,0.,0.,0.);
  float t = 0.0;

  int rowCount = 4;
  int cellRate = 1;
  for(int i = 0; i < beatTotalCount/4; i+= 4){
    t = i/rowCount;
    vec2 thing = vec2(rand(vec2(0.45+0.03 * mod(i, rowCount), 0.03*t+0.05)), rand(vec2(0.4, i)));
    x += buildCell(uv, thing , 0);
  }

  return x;
}

vec4 theCellLife(vec2 uv, vec2 point){
  vec4 x  = vec4(0.,0.,0.,0.);
  float t=0.0;
  float i=0;
  float rowCount = 1;
  float person = 1.0;
  float rate = 0.3;
  float invBeatTotal = TOTAL_BEATS-beatTotalCount;
  float p;
  float cellBoundries;
  float glow;

  p = 4.0; //interesting values @ 8
  cellBoundries = 0.00005 + iOvertoneVolume * 0.0003;

  float f = texture2D(iChannel0, vec2((4096.0/4096.0)*uv.y,0.25)).x;
  glow = 0.001 + (f * 0.003) + (iOvertoneVolume*0.02);

  vec2 pix = mod(gl_FragCoord.xy, vec2(90.0)) - vec2(30.0);
  //cell =  abs(sqrt(pow(uv.x-point.x,p)+pow(uv.y-point.y, p)));

  float snail = 0.0;

  if(iOvertoneVolume > 0.01){
    if(snail == 0.0){
      if(beatTotalCount > 64.0){
        point.y -= 0.2+0.5*sin(beatTotalCount*0.009/point.x)*1/invBeatTotal-0.2 *  rand2(vec2(point.y,point.x));
      }
      else{
        point.y -= (0.2+0.5*sin(invBeatTotal*0.009/point.x))*1/beatTotalCount;
      }
    }
    else{
      point.y -= 0.2+0.5*sin(beatTotalCount*0.009/point.x)*1/invBeatTotal-0.2;
    }

  }

  float cell =  abs(sqrt(pow(uv.x-point.x,p)+pow(uv.y-point.y, p)));
  // cell = cell  * (0.5+0.5*sin(iGlobalTime/0.55)) +  dot(vec2(uv.x-point.x,uv.y-point.y), vec2(uv.x-point.x, uv.y-point.y)) * (0.5+0.5*sin(iGlobalTime*0.55 + pi*2));

  if (cell > cellBoundries){
    person -= 1.0;
  }else {
     person -= 0.9 - cell*100;
  }
  vec4 helloPoint = vec4(vec3(person), 1.0);
  vec4 glowing = vec4(0.0);
  float res = glow / length(point - uv);
  glowing = res * vec4(measureCount*iOvertoneVolume, 0.1, 1/iGlobalTime, 1.0);

  return helloPoint + glowing * 0.3;
}

vec4 cellSpell(vec2 uv){
  vec4 r = vec4(0.0);
  vec2 position;
  float cells=1.0;

  if(iNycWeight >= 2.0){
    if(iOvertoneVolume > 0.01){
      cells = max(1.0, 2*texture2D(iChannel0, vec2(0.0,0.25)).x);
    }
    cells = clamp(cells, 1.0, 30);
  }

  for(int i=0; i < cells; i++){
    if(i==0){
      position = vec2(0.5, iResolution.x*0.00025);
    }
    else{
      position = vec2(rand(vec2(1/iGlobalBeatCount*0.1,i/iGlobalTime*0.1)) * 0.5 + 0.25,
                      rand(vec2(i*0.1,i*0.1)) + 0.15);
    }
    r += theCellLife(uv, position);
  }
  return r;
}

void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;

  vec4 populationResult  = vec4(0.0,0.0,0.0,0.0);
  vec4 circleResult      = vec4(0.0,0.0,0.0,0.0);
  vec4 bouncingResult    = vec4(0.0,0.0,0.0,0.0);
  vec4 cellSpellResult   = vec4(0.0,0.0,0.0,0.0);
  vec4 circleDanceResult = vec4(0.0,0.0,0.0,0.0);

  if(iBouncingWeight > 0.0){
    bouncingResult = bouncingPerson(uv);
    bouncingResult = 2/bouncingResult;

    if(iInvertColor == 0.0 || iCircularWeight == 1.0 && iBouncingWeight >= 4.0){
      bouncingResult =  1.0-bouncingResult;
    }
    else if(iInvertColor == 1.0){
      if(mod(iGlobalBeatCount, 256) > 128){
        bouncingResult = 1 - bouncingResult;
      }
    }
  }

  vec4 c;

  if(iCircularWeight > 0.0){
    circleResult = min(1.0, iCircularWeight) * circular();
  }

  if(iPopulationWeight > 0.0){
    populationResult = min(1.0, iPopulationWeight)*populationDensity(uv);
  }

  //  if(iNycWeight > 0.0){
    //    cellSpellResult = cellSpell(uv);
  //  }

  if(iCircleDanceWeight > 0.0 && iBouncingWeight == 0.0 && iCircularWeight == 0.0){
    circleDanceResult = circleDance();
  }

  c = (cellSpellResult + populationResult + circleResult + bouncingResult);

  gl_FragColor = lineDistort(c, uv) + circleDanceResult;
}

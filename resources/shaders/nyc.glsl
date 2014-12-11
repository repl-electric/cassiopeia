//NYC
uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iBeatCount;
uniform float iMeasureCount;

uniform float iCubeCount;
uniform float iCircleCount;
uniform float iAccelerator;
uniform float iColor;
uniform float iScale;
uniform float iHalfPi;
uniform float iInOutSpeed;

uniform float iCircularWeight;
uniform float iFlareWeight;
uniform float iPopulationWeight;
uniform float iBouncingWeight;

uniform bool iInvertColor;

uniform float iSnowRatio;
uniform float iDestructure;

uniform float iGlobalBeatCount;
uniform float iBeatTotalCount;

const float pi = 3.14159265;
const mat2 m = mat2(0.80,  0.60, -0.60,  0.80);

const float darkMode = 0.0;

#define FLARE_SIZE 10

#define RANDOM_LETTERS 0
#define TOTAL_BEATS 128.0
#define STATIC_LETTERS 0
#define SHOW_GLOW 1
#define PANIC 0

vec3 hsv2rgb(float h, float s, float v) {
  return mix(vec3(1.), clamp((abs(fract(h+vec3(3.,2.,1.)/3.)*6.-3.)-1.),0.,1.),s)*v;
}

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float rand2(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(float x, float y){return sin(1.5*x)*sin(1.5*y);}

mat2 mm2(in float a){
  float c = abs(cos(a));
  float s = sin(a);
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  float FREQ_SCALE = (4096.0/4096.0);
  float AMP_SCALE = 1.0/2.0;
  float fi = FREQ_SCALE*uv.y;
  float fid  = FREQ_SCALE/4096.0/2.0;
  float adjust = AMP_SCALE * 0.5 *   (max(0.0, texture2D(iChannel0, vec2(fi,0.25)).x) +
                                      max(0.0, texture2D(iChannel0, vec2(fi+fid,0.25)).x));

  return mat2(c * adjust, -s * adjust,s * adjust ,c * adjust);
}

float saturate(float a){ return clamp( a, 0.0, 1.0 );}
// Fractional Brownian Motion code by IQ.
// http://en.wikipedia.org/wiki/Fractional_Brownian_motion

float fbm4( float x, float y ){
  vec2 p = vec2( x, y );
  float f = 0.0;

  if(iBeat == 1.0){
    f = .02;
  }

  f +=  0.5000*noise(p.x, p.y);
  p = m*p*2.02 + iMeasureCount * 0.0009;
  f +=  0.2500*noise(p.x, p.y);
  p = m*p*2.03 + iMeasureCount * 0.0009;
  f +=  0.1250*noise(p.x, p.y);
  p = m*p*2.01 + iMeasureCount * 0.0009;
  f +=  0.0625*noise(p.x, p.y);

  return f / (0.9375 + (iMeasureCount * 0.009 * 4));
}

const float linesmooth = 0.0333;

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

vec4 circular(void){
  vec2 mainuv = (gl_FragCoord.xy / iResolution.xy);

  float aspect = iResolution.x/iResolution.y;
  float finalval = iColor; // 0.1
  float scale = iScale;
  float speed = iAccelerator;
  float shading = 0.20025;
  //relate speed - shading
  float halfpi = iHalfPi;

  float circleScale = iCircleCount;
  if (iOvertoneVolume < 0.01) {
    circleScale = 0.0;
    speed = 0.0;
    halfpi = 0.000000001;
  }

  vec2 uv = mainuv * scale - scale * 0.5;
  uv.x *= aspect;

  float mainval = 1.0; //50-60 nice overpaint effect
  float inverseLength;

  if(iDestructure == 1.0){
    inverseLength = saturate(length(uv)) + clamp(iBeat, 0.0, 0.002);
  }
  else{
    inverseLength = saturate(length(uv)) * (uv.x + uv.y) *  iDestructure;
  }

  //inverseLength = saturate(length(uv)) * rand(vec2(texture2D(iChannel0, vec2(0, 0)).x, texture2D(iChannel0, vec2(0, 0)).y));

  float core = inverseLength * circleScale;
  float coreident = ceil(core);

  speed = 1e-4 + clamp(iBeat,0.0,0.0000001);

  shading = clamp(iOvertoneVolume, 0.20025, 0.20025);

  float musiz = texture2D(iChannel0, vec2(0.,0.)).x / 100.0 + iOvertoneVolume/1;
  //
  //vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.005 , iGlobalTime * 0.005) * pi * pi );
  //rotatedUVs *= mm2( halfpi - fbm4( coreident * 2.0 , iGlobalTime * 0.05) * pi * pi )  * musiz;

  //  * texture2D(iChannel0, vec2(0, 0)).x

  vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.005 , iGlobalTime * 0.07 * clamp(texture2D(iChannel0, vec2(0, 0)).x, 0.0, 0.2)) * pi * pi );
   rotatedUVs *= mm2( halfpi - fbm4(coreident * 2.0 , iGlobalTime *  0.1 * clamp(texture2D(iChannel0, vec2(0, 0)).x, 0.0, 1.0) ) * pi * pi );

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
  const float edgethreshold = 0.1;
  mainval *= smoothstep(edgethreshold - corewidth, edgethreshold + corewidth, coresmooth);
  finalval += mainval;

  finalval = max(finalval, 0.0) + 0.0025;
  finalval = min(finalval, 1.0);

  return vec4(vec3(pow(finalval, 1.0/2.0)) - iBeat * 0.1, 1.0);
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


// original by nimitz https://www.shadertoy.com/view/lsSGzy#, slightly modified

#define ray_brightness 0.8
#define gamma 0.1

#define curvature 15.
#define red   4.
#define green 1.0
#define blue  .1

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!!!!!!!!!!! UNCOMMENT ONE OF THESE TO CHANGE EFFECTS !!!!!!!!!!!
// MODE IS THE PRIMARY MODE
#define MODE normalize
//#define MODE

#define MODE3 +
//#define MODE3 +

#define MODE2 r +
//#define MODE2

#define DIRECTION +
//#define DIRECTION -

#define SIZE 0.1

#define INVERT /
//#define INVERT *
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

float noise( in vec2 x )
{
  return texture2D(iChannel2, x*.01).x; // INCREASE MULTIPLIER TO INCREASE NOISE
}

// FLARING GENERATOR, A.K.A PURE AWESOME
mat2 m2 = mat2( 0.80,  0.60, -0.60,  0.80 );
float fbm( in vec2 p )
{
  float z=5.;       // EDIT THIS TO MODIFY THE INTENSITY OF RAYS
  float rz = 0.09; // EDIT THIS TO MODIFY THE LENGTH OF RAYS
  p *= 0.25;        // EDIT THIS TO MODIFY THE FREQUENCY OF RAYS
  for (int i= 1; i < 6; i++)
    {
      rz+= abs((noise(p)-0.5)*2.)/z;
      z = z*2.;
      p = p*2.*m2;
    }
  return rz;
}

vec4 flare(void)
{
  float ray_density = max(4.5, 0.9*texture2D(iChannel0, vec2(0.0,0.25)).x);

  float t = DIRECTION iGlobalTime*.3;
  vec2 uv = gl_FragCoord.xy / iResolution.xy-0.5;
  uv.x *= iResolution.x/iResolution.y;
  uv*= curvature* SIZE;

  float r = sqrt(dot(uv,uv)); // DISTANCE FROM CENTER, A.K.A CIRCLE
  float x = dot(MODE(uv), vec2(.5,0.))+t;
  float y = dot(MODE(uv), vec2(.0,.5))+t;

  float val;
  val = fbm(vec2(MODE2 y * ray_density, MODE2 x MODE3 ray_density)); // GENERATES THE FLARING
  val = smoothstep(gamma*.02-.1,ray_brightness+(gamma*0.02-.1)+.001,val);
  val = sqrt(val); // WE DON'T REALLY NEED SQRT HERE, CHANGE TO 15. * val FOR PERFORMANCE

  vec3 col = val INVERT vec3(0.2, 0.1 + 0.01*iBeat,0.1+iBeat*0.01);
  //  col = 0.-col; // WE DO NOT NEED TO CLAMP THIS LIKE THE NIMITZ SHADER DOES!
  float rad = 0.2 * texture2D(iChannel0, vec2(0,0.25)).x; // MODIFY THIS TO CHANGE THE RADIUS OF THE SUNS CENTER
  col = mix(col,vec3(1.), rad - 266.667 * r); // REMOVE THIS TO SEE THE FLARING

  return (vec4(col, 1) - vec4(1.8, 1.9, 1.9, 0));
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

      float y1;
      int converge = 1;
      if(converge == 1){
        y1 = 0.5+0.5*sin(iBeatTotalCount*0.05);
      }
      else{
        y1 = 1;
      }

      if(iBeatTotalCount > 64.0){
        point.y -= y1 * sin(iBeatTotalCount*0.04/point.x);
      }
      else{
       point.y -= y1 * sin((TOTAL_BEATS-iBeatTotalCount)*0.04/point.x);
      }
    }

    //point.x += sin(iBeatTotalCount*0.1)*0.5;
  }

  float p;
  float cellBoundries;
  float glowFactor;

  //round cells
  p = 2.;
  cellBoundries = 0.5;
  glowFactor = 0.04;

  //square cells
  //  p = 4.0;
  //  cellBoundries = 0.0001;
  //  glowFactor = 0.003;

  float cell = smoothstep(sqrt(pow(uv.x-point.x,p)+pow(uv.y-point.y, p)),
                          0.01+sqrt(pow(uv.x-point.x,p)+pow(uv.y-point.y, p)),
                          1.0);

  if (cell > cellBoundries){
    person -= 1.0;
  }else if (cell < cellBoundries){
    person -= 0.9;
  }
  vec4 helloPoint = vec4(vec3(person),1.0);

  if(SHOW_GLOW==1){
    helloPoint += addGlow(uv, point, glowFactor);
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
        helloPoint += buildCell(uv, point, STATIC_LETTERS);
      }
    }
  }
  return helloPoint;
}

vec4 bouncingPerson(vec2 uv){
  float letterSpace = 0.06;
  float top = 0.6;
  vec4 helloPoint = vec4(0.0);

  mat3 complete = mat3(1, 1, 1,  1, 1, 1,  1, 1, 1);
  mat3 letterR  = mat3(1, 1, 1,  1, 1, 0,  1, 0, 1);
  mat3 letterE  = mat3(1, 1, 1,  1, 1, 0,  1, 1, 1);
  mat3 letterP  = mat3(1, 1, 1,  1, 1, 1,  1, 0, 0);
  mat3 letterL  = mat3(1, 0, 0,  1, 0, 0,  1, 1, 1);

  mat3 letterC  = mat3(1, 1, 1,  1, 0, 0,  1, 1, 1);
  mat3 letterT  = mat3(1, 1, 1,  0, 1, 0,  0, 1, 0);
  mat3 letterI  = mat3(0, 1, 0,  0, 1, 0,  0, 1, 0);


  helloPoint += letter(letterR, vec2(0.3+letterSpace*0, top), uv);

  if(iOvertoneVolume > 0.1){
    helloPoint += letter(letterE, vec2(0.3+letterSpace*2, top), uv);

    if(PANIC == 0){
      helloPoint += letter(letterP, vec2(0.3+letterSpace*4, top), uv);
      helloPoint += letter(letterL, vec2(0.3+letterSpace*6, top), uv);
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

      helloPoint += letter(invertedR, vec2(0.3+letterSpace*0, top), uv) * liveUntil;
      helloPoint += letter(invertedE, vec2(0.3+letterSpace*2, top), uv) * liveUntil;
      helloPoint += letter(invertedP, vec2(0.3+letterSpace*4, top), uv) * liveUntil;
      helloPoint += letter(invertedL, vec2(0.3+letterSpace*6, top), uv) * liveUntil;
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

vec4 theCellLife(vec2 uv, vec2 point){
  vec4 x  = vec4(0.,0.,0.,0.);
  float t=0.0;
  float i=0;
  float rowCount = 1;
  float person = 1.0;
  float inc;
  float movementScale = .0000001;
  float rate = 0.3;
  float invBeatTotal = TOTAL_BEATS-iBeatTotalCount;
  float p;
  float cellBoundries;
  float glow;

  p = 4.0; //interesting values @ 8
  cellBoundries = 0.0002 + iOvertoneVolume * 0.0003;

  float f = texture2D(iChannel0, vec2((4096.0/4096.0)*uv.y,0.25)).x;
  glow = 0.001 + (f * 0.003) + (iOvertoneVolume*0.02);

  vec2 pix = mod(gl_FragCoord.xy, vec2(90.0)) - vec2(30.0);
  //cell =  abs(sqrt(pow(uv.x-point.x,p)+pow(uv.y-point.y, p)));

  float snail = 0.0;

  if(iOvertoneVolume > 0.01){
    if(snail == 0.0){
      if(iBeatTotalCount > 64.0){
        point.y -= 0.2+0.5*sin(iBeatTotalCount*0.009/point.x)*1/invBeatTotal-0.2 *  rand2(vec2(point.y,point.x));
      }
      else{
        point.y -= (0.2+0.5*sin(invBeatTotal*0.009/point.x))*1/iBeatTotalCount;
      }
    }
    else{
      point.y -= 0.2+0.5*sin(iBeatTotalCount*0.009/point.x)*1/invBeatTotal-0.2;
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
  glowing = res * vec4(iMeasureCount*iOvertoneVolume, 0.1, 1/iGlobalTime, 1.0);

  return helloPoint + glowing * 0.3;
}

vec4 cellSpell(vec2 uv){
  vec4 r = vec4(0.0);
  vec2 position;
  float cells=1.0;

  if(iOvertoneVolume > 0.01){
    cells = max(1.0, 2*texture2D(iChannel0, vec2(0.0,0.25)).x);
  }
  //cells = clamp(cells, 1.0, 10);
  cells = clamp(cells, 1.0, 70);
    //  cells = texture2D(iChannel0, vec2(0.25, 0.25)).x;

  for(int i=0; i < cells; i++){
    if(i==0){
      position = vec2(0.5, 0.5);
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

  float snowWeight = 0.0;
  float flareWeight = iFlareWeight; //0.01;
  float populationWeight = iPopulationWeight;
  float circularWeight = iCircularWeight;
  float spellWeight = 0.0;
  float bouncingWeight = iBouncingWeight;
  float cellSpellWeight = 1.0;

  float darkMode = 0.0;

  float snowSpeed = 0.000000000001; //0.00000001; //0.0000000001;
  vec4 populationResult = vec4(0., 0., 0., 0.);
  vec4 circleResult = vec4(0., 0., 0., 0.);
  vec4 flareResult = vec4(0., 0., 0., 0.);
  vec4 bouncingResult = vec4(0., 0., 0., 0.);
  vec4 snowResult = vec4(.0,.0,.0,.0);
  vec4 cellSpellResult = vec4(.0,.0,.0,.0);

  if(iOvertoneVolume > 0.01){
    snowSpeed = 0.0000000001 + (iBeat * 0.00000000000009);
    snowWeight = 0.3;
  }
  else{
    snowWeight = 0.0;
    circularWeight = 0.0;
  }

  if(populationWeight == 1.0){
    snowSpeed = 0.00000000001;
    snowWeight = 0.4;
  }

  vec4 spelling = vec4(0.);
  if(spellWeight == 1.0){
    spelling = bouncingPerson(uv);
  }

  if(bouncingWeight > 0.0){
    bouncingResult = bouncingPerson(uv);
    bouncingResult = 2/bouncingResult;

    if(iInvertColor==false){
      bouncingResult = 1 - bouncingResult;
    }
    else{
      if(mod(iGlobalBeatCount,256) > 128){
        bouncingResult = 1 - bouncingResult;
      }
    }
  }

  snowSpeed *= iSnowRatio;

  vec4 c;

  if(circularWeight > 0.0){
    circleResult = circularWeight*circular(); //- (snowWeight*generateSnow(uv, snowSpeed));
  }

  if(populationWeight > 0.0){
    populationResult = populationWeight*populationDensity(uv);
  }

  if(flareWeight > 0.0){
    flareResult = 0.01*flare();
    flareResult = 1-(FLARE_SIZE*flareResult - bouncingPerson(uv));
    //    flareResult *= 0.1;
  }

  if(snowWeight > 0.0){
    snowResult = (1.0-(snowWeight * generateSnow(uv, snowSpeed)));
  }

  if(cellSpellWeight > 0.0){
    cellSpellResult = cellSpell(uv);
  }

  if(darkMode == 1.0){
    c = 1.0-(circularWeight*circular()) -  snowResult - populationResult;
  }
  else{
    c = (cellSpellResult +
         populationResult +
         circleResult +
         bouncingResult +
         flareResult);
  }
  gl_FragColor = lineDistort(c, uv);
}

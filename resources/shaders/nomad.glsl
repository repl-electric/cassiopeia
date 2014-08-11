float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
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

vec4 buildNoise(float pos, float direction)
{
  vec2 position = pos-gl_FragCoord.xy / iResolution.xy;
  position = position + vec2(1/iResolution.x, iResolution.y);
  vec4 colour = vec4(0.,0.,0.,0.);
  float random = rand(vec2(position.x/direction + iGlobalTime/100.0, position.y));
  random = random - 0.9;
  float randomMultiplier =position.x/direction * 10.1;
  colour += random * randomMultiplier;
  float multiplier = (position.x / 0.05);

  if(pos == 0.0){
    colour += multiplier;
  }
  else{
    colour -= multiplier;
  }
  return mix(colour, vec4(0.0,0.,0.,0.), 0.5);
}

// Based on inigo quilez Hex - iq/2014
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

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


vec4 hex( void )
{
  vec2 uv = gl_FragCoord.xy/iResolution.xy;
  vec2 pos = (-iResolution.xy + 2.0*gl_FragCoord.xy)/iResolution.y;

  float scale = 10.0;
  float speed = sin(iGlobalTime * 0.0001);

  // gray
  vec4 h = hexagon(scale*pos + 0.1);
  float n = noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) );
  vec3 col = 0.15 + 0.15*hash1(h.xy+1.2)*vec3(1.0);
  col *= smoothstep( 0.10, 0.11, h.z );
  col *= smoothstep( 0.10, 0.11, h.w );
  col *= 1.0 + 0.15*sin(40.0*h.z);
  col *= 0.75 + 0.5*h.z*n;

  // red
  h = hexagon(scale*pos + speed*iGlobalTime);
  n = noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) );
  vec3 colb = 0.9 + 0.8*sin( hash1(h.xy)*1.5 + 2.0 + vec3(0.0,1.0,1.0) );
  colb *= smoothstep( 0.10, 0.11, h.z );
  colb *= 1.0 + 0.15*sin(40.0*h.z);
  colb *= 0.75 + 0.5*h.z*n;

  h = hexagon(scale* (pos+0.1*vec2(-1.3,1.0)) + speed*iGlobalTime);
  col *= 1.0-0.8*smoothstep(0.45,0.451,noise( vec3(0.3*h.xy+iGlobalTime*0.1,iGlobalTime) ));

  col = mix( col, colb, smoothstep(0.45,0.451,n) );
  col *= pow( 16.0*uv.x*(1.0-uv.x)*uv.y*(1.0-uv.y), 0.1 );
  return vec4( col, 1.0 );
}

void main(void){
  vec4 leftNoise  = buildNoise(0.,1);
  vec4 rightNoise = buildNoise(1.,-1);
  gl_FragColor = 1-(rightNoise * leftNoise) * hex();
}

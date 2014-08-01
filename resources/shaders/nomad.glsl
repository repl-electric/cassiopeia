float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float averageForRadius(vec2 co, float radius) {
  float average = float(0);
  float sampleSize = 5.0;
  float y2 = 0.0;
  float x2;
  float x3;

  x2 = sqrt(pow(radius, 1.0) - pow(y2, 2.0));
  x3 = (x2 - sampleSize) * -2.0;
  y2 += sampleSize;
  return average;
}

vec4 buildNoise(float pos)
{
  vec2 position = pos-gl_FragCoord.xy / iResolution.xy;
  position = position + vec2(1/iResolution.x, iResolution.y);
  vec4 colour = vec4(0.,0.,0.,0.);
  float random = rand(vec2(position.x + iGlobalTime/100.0, position.y));
  random = (random * 1.5) - 1.0;
  float randomMultiplier = 1.0 - position.x * 0.0;
  colour += random * randomMultiplier;
  float multiplier = (position.x / 0.01);

  if(pos == 0.0){
    colour += multiplier;
  }
  else{
    colour -= multiplier;
  }
  return mix(colour, vec4(0.0,0.,0.,0.), 0.01);
}

void main(void){
  vec4 leftNoise  = buildNoise(0);
  vec4 rightNoise = buildNoise(1);
  gl_FragColor = 1.0-(rightNoise * leftNoise);
}

// by srtuss, 2013
// did some research on kali's "comsos" and came up with this.
// as always, not optimized, just pretty :)
//
// name & inspiration was taken from here:
// http://www.youtube.com/watch?v=BzQmeeXcDwQ

vec2 rotate(vec2 p, float a)
{
  return vec2(p.x * cos(a) - p.y * sin(a), p.x * sin(a) + p.y * cos(a));
}

void main(void)
{
  vec2 uv = gl_FragCoord.xy / iResolution.xy;
  uv = uv * 2.0 - 1.0;
  uv.x *= iResolution.x / iResolution.y;

  float v = 0.0;

  vec3 ray = vec3(sin(iGlobalTime * 0.1) * 0.2, cos(iGlobalTime * 0.13) * 0.2, 1.5);
  vec3 dir = normalize(vec3(uv, 1.0));

  ray.z += iGlobalTime * 0.1 - 20.0;
  dir.xz = rotate(dir.xz, sin(iGlobalTime * 0.1) * 2.0);
  dir.xy = rotate(dir.xy, iGlobalTime * 0.2);

  // very little steps for the sake of a good framerate
  #define STEPS 20

  float inc = 0.35 / float(STEPS);

  vec3 acc = vec3(0.0);

  for(int i = 0; i < STEPS; i ++)
    {
      vec3 p = ray * 0.1;

      // do you like cubes?
      //p = floor(ray * 20.0) / 20.0;

      // fractal from "cosmos"
      for(int i = 0; i < 14; i ++)
        {
          p = abs(p) / dot(p, p) * 2.0 - 1.0;
        }
      float it = 0.001 * length(p * p);
      v += it;

      // cheap coloring
      acc += sqrt(it) * texture2D(iChannel0, ray.xy * 0.1 + ray.z * 0.1).xyz;

      ray += dir * inc;
    }

  // old blueish colorset
  /*vec3 ex = 4.0 * vec3(0.9, 0.3, 0.1);
    gl_FragColor = vec4(pow(vec3(v), ex), 1.0);*/

  float br = pow(v * 4.0, 3.0) * 0.1;
  vec3 col = pow(acc * 0.5, vec3(1.2)) + br;
  gl_FragColor = vec4(col, 1.0);
}

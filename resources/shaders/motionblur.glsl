uniform float iOvertoneVolume;
uniform float iLColor;
uniform float iRColor;
uniform float iA;
uniform float iG;
uniform float iRes;
uniform float iSpace;

void sampleCamera(vec2 u, out vec3 rayOrigin, out vec3 rayDir)
{
  vec2 filmUv = (gl_FragCoord.xy + u)/iResolution.xy;

  float tx = (2.0*filmUv.x - 1.0)*(iResolution.x/iResolution.y);
  float ty = (1.0 - 2.0*filmUv.y);
  float tz = 0.0;

  rayOrigin = vec3(0.0, 0.0, 5.0);
  rayDir = normalize(vec3(tx, ty, tz) - rayOrigin);
}

float Q(float a, float b, float c)
{
  float d = b*b-4.0*a*c;
  if (d < 0.0) return -1.0;
  d=sqrt(d);
  float oo2a = 0.5/a;
  return min((-b-d)*oo2a,(-b+d)*oo2a);
}

vec3 L = normalize(vec3(-2,-1,1));

float Lit(vec3 N, vec3 V, vec3 H)
{
  float d = max(dot(N,L),0.0) * 0.5;
  float s = pow(max(dot(N,H),0.0),1000.0)*8.0;

  return d+s;
}

vec3 trace(vec3 P, vec3 V, vec3 H, float time)
{
  time += iGlobalTime;
  float t2 = sin(time*.1)*40.0;
  time += sin(time)*0.9;
  vec3 c=vec3(0.2,0.1,0.1);

#define N_SPHERE 4

  vec4 S[N_SPHERE];
  vec3 C[N_SPHERE];
  vec2 gb=vec2(0.1,0);

  for (int i=0; i<N_SPHERE; i++)
    {
      float I = float(i)*(1.0/float(N_SPHERE));
      float t = iOvertoneVolume*I*2.0 + time;
      vec3 A = vec3(sin(t+t2) * iOvertoneVolume,
                    sin(t*3.0)* iA,
                    cos(t+t2) * iG);
      float R = (0.3-I)*0.33 + 0.1;
      S[i]=vec4(A,R);

      C[i]=vec3(0.1,gb);
      gb=vec2(1.0,1.0)-gb;

    }

  //S[0]=vec4(sin(iGlobalTime),0,cos(iGlobalTime),0.7);
  //S[1]=vec4(sin(iGlobalTime+1.0),0,cos(iGlobalTime+1.0),0.2);

  float nearest = 1e10;

  vec3 E=V;

  for (int i=0; i<N_SPHERE; i++)
    {
      vec3 A=S[i].xyz;
      float R=S[i].w;
      //float I = float(i)*(1.0/float(N_SPHERE));
      //float t = I*2.0*3.1415927 + iGlobalTime;
      //vec3 A = vec3(sin(t),sin(t*3.0)*0.1,cos(t));
      //float R = I*0.5 + 0.1;
      float T=Q(dot(V,V),2.0*(dot(P,V)-(dot(A,V))),dot(A,A)+dot(P,P)-R*R-(2.0*(dot(A,P))));
      if (T > 0.0)
        {
          if (T < nearest)
            {
              vec3 X=P+T*V;
              vec3 N=normalize(X-A);

              vec3 Ref = reflect(V,N);

              float nearestR = 1e10;
              vec3 Rcol=vec3(0.0,0.0,0.0);
              int blocked =0;
              for (int j=0; j<N_SPHERE; j++)
                {
                  if (i!=j)
                    {
                      vec3 A=S[j].xyz;
                      float R=S[j].w;
                      vec3 V=L;
                      vec3 P=X;
                      float T=Q(dot(V,V),2.0*(dot(P,V)-(dot(A,V))),dot(A,A)+dot(P,P)-R*R-(2.0*(dot(A,P))));
                      if (T > 0.0)
                        blocked=1;

                      V=normalize(Ref);
                      T=Q(dot(V,V),2.0*(dot(P,V)-(dot(A,V))),dot(A,A)+dot(P,P)-R*R-(2.0*(dot(A,P))));
                      if (T > 0.0)
                        {
                          if (T<nearestR)
                            {
                              vec3 X=P+T*V;
                              vec3 N=normalize(X-A);

                              nearestR=T;
                              Rcol=C[j]+Lit(N,E,H);
                              Rcol *= 0.5/(1.0+T*T);
                            }
                        }
                    }
                }

              //vec3 H=normalize(L-V);
              //float d = max(dot(N,L),0.0) * 0.5;
              //float s = pow(max(dot(N,H),0.0),100.0)*.5;

              float b = blocked > 0 ? 0.0 : Lit(N,V,H); //d+s;
              nearest = T;


              c = vec3(b,b,b)+C[i]+Rcol;
            }
        }
    }

  return c;
}

float hash(float n)
{
  return fract(sin(n)*43758.5453123);
}

float rand( vec2 n )
{
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* 43758.5453);
}

/*
int wang_hash(int seed)
{
    seed = (seed ^ 61) ^ (seed >> 16);
    seed *= 9;
    seed = seed ^ (seed >> 4);
    seed *= 0x27d4eb2d;
    seed = seed ^ (seed >> 15);
    return seed;
}
*/

void main(void)
{
  vec3 P, V;
  sampleCamera(vec2(0.5,0.5), P, V);

  vec3 H=normalize(L-V);

  vec3 c = vec3(0.0,0.0,0.0);

  vec2 ditheruv = floor(gl_FragCoord.xy)+vec2(.5);

  float p = fract(iGlobalTime)*123.789 + gl_FragCoord.y*iResolution.x+gl_FragCoord.x;
  float spread = 1.0/20.0;

  float divider = gl_FragCoord.x - iMouse.x;

  const int iterations = 8;
  float t = texture2D(iChannel0,ditheruv/256.0,-100.0).x*spread/float(iterations);
  float delta = spread/float(iterations);

  if ( divider < 0.0 )
    {
      t = spread*.5;
      delta = 0.0; // disable motion blur
    }

  for ( int i=0; i < iterations; i++ )
    {
      c += trace(P,V,H, t);
      t += delta;
    }

  c /= float(iterations);

  if ( abs(divider) <= 1.0 )
    {
      c = vec3(1,0,0);
    }

  gl_FragColor = vec4(c,1.0);
}

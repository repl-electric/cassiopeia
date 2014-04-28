/*by musk License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

  messing around with my previous shader . . .


*/

//bounce count drasticaly reduces performance for long simulations
//setting them to zero disables bouncing but drastically improves framerate
#define max_floor_bounce 3
#define max_wall_bounce 4

//motion blur! reccomended
#define motion_blur

//nr of particles,
//try with 100, 200, 10, 1000
#define samples 5

//brightness conntrols the how much light each particle emits
//reduce brightness if you put in more particles
#define brightness .9

//how many seconds you want each simulation to run
//try with 4.0, 2.0, 1.0, 0.5, 0.33, 0.2
#define time_per_simulation 8.0

//time offset to get a nice screenshot ;)
#define time_offset -5.75

//solve 2nd order polynomial, return the maximum of two solutions
//could have optimised for this particular example... meh...
float second(float a,float b,float c)
{
  float x1 = (-b+sqrt((b*b-4.0*a*c)))/(2.0*a);
  float x2= (-b-sqrt((b*b-4.0*a*c)))/(2.0*a);
  return max(x1,x2);
}

//given initial values compute position after t seconds
vec3 physics(vec3 pos, vec3 vel, vec3 acc, float t)
{
  //this loop processes upto max_bounces collisions... nice :)
  for (int i=0; i<max_floor_bounce; i++)
    {
      float tc = second(acc.y*.5,vel.y,pos.y);
      //now we know that there will be a collision with the plane
      //in exactly tc seconds

      if (t>tc) //if time is greater than time of collision
        {
          t-=tc; //process the collision
          pos = pos + vel*tc + acc*tc*tc*.5;
          vel = vel + acc*tc;
          vel.y*=-.5; //make it bounce
          vel.x=vel.x*.8+sin(pos.x*4.0)*length(vel)*.5;
        }
      else break; //it wont collide, yay!
    }

  pos = pos + vel*t + acc*t*t*.5; // x = v*t + .5*a*t^2

  float ar = iResolution.x/iResolution.y;
  float hwall = 8.0*ar;

  for (int i=0; i<max_wall_bounce; i++)
    {
      if (pos.x>+hwall) pos.x = 2.0*hwall-pos.x;
      else if (pos.x<-hwall) pos.x = -2.0*hwall-pos.x;
      else break;
    }
  return pos;
}

float hash(float x)
{
  return fract(sin(x*.0127863)*17143.321); //decent hash for noise generation
}

float hash(vec2 x)
{
  return fract(cos(dot(x.xy,vec2(2.31,53.21))*124.123)*412.0);
}

vec3 cc(vec3 color, float factor,float factor2) // color modifier
{
  float w = color.x+color.y+color.z;
  return mix(color,vec3(w)*factor,w*factor2);
}

void main(void)
{
  vec2 uv = gl_FragCoord.xy / iResolution.xy -.5;
  uv.x *= iResolution.x/iResolution.y;

  uv*=16.0;

  float acc = .0;

  float t = iGlobalTime + time_offset;

  //new simulation after every time_per_simulation seconds
  float mt = mod(t,time_per_simulation);
  float seed = t-mt; //the seed to generate new variables

  for (int sample = 0; sample<samples; sample++)
    {
      #ifdef motion_blur
      float tnoise = hash(uv.xy+vec2(sample,sample))*(1.0/12.0);
      #else
      float tnoise = .0;
      #endif

      float iseed = seed + float(sample); //the seed for each particle

      float iangle = 3.14159*2.0*hash(iseed);
      float imagnitude = 64.0*hash(iseed+15.420)*hash(seed+14.2)+4.0;
      float angle = 3.14159*2.0*hash(seed);
      float magnitude = 64.0*hash(seed);
      vec2 pos = vec2(hash(seed+2.0),hash(seed+3.0))*8.0-4.0;

      vec3 p = (
                physics(
                        vec3(pos.x,pos.y+8.0,8.0), //initial position
                        vec3(cos(iangle),sin(iangle),.0)*imagnitude+
                        vec3(cos(angle),sin(angle),.0)*magnitude,
                        vec3(.0,-40.0,.0),  //acceleration
                        mt+tnoise ));

      vec2 temp = uv-p.xy+vec2(.0,8.0);
      temp/=iResolution.y/300.0; //adjust the size of the particles
      float s = sqrt(dot(temp,temp));
      //s-=1.0;
      s*=iResolution.y*.05;
      //s = min(1.0,max(.0,s));

      acc+=1.0/(s+1.0);
    }

  vec3 part_color = vec3(cos(seed*13.1230),cos(seed*15.5310),cos(seed*17.55));
  part_color = normalize(abs(part_color));
  vec3 color = vec3(0.1,.1,.3)+part_color*acc*brightness;

  //color = max(color,1.);

  //color = mix(color*color,color,1.4);
  color *=.8;
  color -= length(uv)*.005;
  color = cc(color,.5,.5);

  color += hash(uv.xy+color.xy)*.02;

  gl_FragColor = vec4(color,1.0);
}

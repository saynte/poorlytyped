#include <stdlib.h>
#include <vector>
#include <iostream>
#include <limits>
#include <cmath>
using namespace std;
numeric_limits<double> real;
double delta = sqrt(real.epsilon()), infinity = real.infinity();
struct Vec {
  double x, y, z;
  Vec(double x2, double y2, double z2) : x(x2), y(y2), z(z2) {}
};
Vec operator+(const Vec &a, const Vec &b)
{ return Vec(a.x+b.x, a.y+b.y, a.z+b.z); }
Vec operator-(const Vec &a, const Vec &b)
{ return Vec(a.x-b.x, a.y-b.y, a.z-b.z); }
Vec operator*(double a, const Vec &b) { return Vec(a*b.x, a*b.y, a*b.z); }
double dot(const Vec &a, const Vec &b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
double length(const Vec &a) { return sqrt(dot(a, a)); }
Vec unitise(const Vec &a) { return (1 / sqrt(dot(a, a))) * a; }
struct Hit {
  double lambda;
  Vec normal;
  Hit() : lambda(infinity), normal(Vec(0, 0, 0)) {}
};
struct Sphere;
struct Scene {
  virtual ~Scene() {};
  virtual void intersect(Hit &, const Vec &) const = 0;
  virtual bool intersect(const Vec &, const Vec &) const = 0;
  virtual Sphere bound(Sphere b) const = 0;
};
struct Sphere : public Scene {
  Vec center;
  double radius;
  Sphere(Vec c, double r) : center(c), radius(r) {}
  ~Sphere() {}
  double ray_sphere(const Vec &dir) const {
    double b = center.x*dir.x + center.y*dir.y + center.z*dir.z;
    double disc = b*b - dot(center, center) + radius * radius;
    if (disc > 0) {
      double d = sqrt(disc), t2 = b + d;
      if (t2 > 0) {
	double t1 = b - d;
	return (t1 > 0 ? t1 : t2);
      }
    }
    return infinity;
  }
  bool sray_sphere(const Vec &orig, const Vec &dir) const {
    Vec v = center - orig;
    double b = dot(v, dir), disc = b*b - dot(v, v) + radius * radius;
    return (disc < 0 ? false : b + sqrt(disc) >= 0);
  }
  void intersect(Hit &hit, const Vec &dir) const {
    double lambda = ray_sphere(dir);
    if (lambda < hit.lambda) {
      hit.lambda = lambda;
      double
	nx = lambda*dir.x - center.x,
	ny = lambda*dir.y - center.y,
	nz = lambda*dir.z - center.z;
      double il = 1/sqrt(nx*nx + ny*ny + nz*nz);
      hit.normal.x = il*nx;
      hit.normal.y = il*ny;
      hit.normal.z = il*nz;
    }
  }
  bool intersect(const Vec &orig, const Vec &dir) const
  { return sray_sphere(orig, dir); }
  Sphere bound(Sphere b) const {
    return Sphere(b.center, max(b.radius, length(center - b.center) + radius));
  }
};
typedef vector<Scene *> Scenes;
struct Group : public Scene {
  Sphere b;
  Scenes child;
  Group(Sphere b, Scenes c) : b(b), child(c) {}
  ~Group() {
    for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
      delete *it;
  }
  void intersect(Hit &hit, const Vec &dir) const {
    if (b.ray_sphere(dir) < hit.lambda)
      for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
	(*it)->intersect(hit, dir);
  }
  bool intersect(const Vec &orig, const Vec &dir) const {
    if (!b.sray_sphere(orig, dir)) return false;
    for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
      if ((*it)->intersect(orig, dir)) return true;
    return false;
  }
  Sphere bound(Sphere b) const {
    Sphere b2 = b;
    for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
      b2 = (*it)->bound(b2);
    return b2;
  }
};
double ray_trace(const Vec &neg_light, const Vec &dir, const Scene &s) {
  Hit hit;
  s.intersect(hit, dir);
  if (hit.lambda == infinity) return 0;
  double g = dot(hit.normal, neg_light);
  if (g < 0) return 0.;
  Vec p = hit.lambda*dir + delta*hit.normal;
  return (s.intersect(p, neg_light) ? 0 : g);
}
Scene *create(int level, Vec c, double r) {
  Scene *s = new Sphere(c, r);
  if (level == 1) return s;
  Scenes child;
  child.reserve(5);
  child.push_back(s);
  double rn = 3*r/sqrt(12.);
  for (int dz=-1; dz<=1; dz+=2)
    for (int dx=-1; dx<=1; dx+=2)
      child.push_back(create(level-1, c + rn*Vec(dx, 1, dz), r/2));
  Sphere b2(c + Vec(0, r, 0), r);
  for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
    b2 = (*it)->bound(b2);
  return new Group(b2, child);
}
int main(int argc, char *argv[]) {
  int level = (argc==3 ? atoi(argv[1]) : 9),
    n = (argc==3 ? atoi(argv[2]) : 512), ss = 4;
  Vec neg_light = unitise(Vec(1, 3, -2));
  Scene *s(create(level, Vec(0, -1, 4), 1));
  cout << "P5\n" << n << " " << n << "\n255\n";
  char* ar = (char*) malloc (sizeof (char) * n * n);

#pragma omp parallel for schedule (static, 1)
  for (int y=n-1; y>=0; --y)
    for (int x=0; x<n; ++x) {
      double g=0;
      for (int dx=0; dx<ss; ++dx)
	for (int dy=0; dy<ss; ++dy) {
	  Vec dir(unitise(Vec(x+dx*1./ss-n/2., y+dy*1./ss-n/2., n)));
	  g += ray_trace(neg_light, dir, *s);
	}
      *(ar + x + n*y) = char(int(.5 + 255. * g / (ss*ss)));
    }
  
  for (int y=n-1; y>=0; --y) {
    for (int x=0; x<n; ++x) {
      cout << *(ar + x + n*y);
    }
  }

  return 0;
}

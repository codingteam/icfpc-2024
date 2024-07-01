#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <array>
#include <tuple>

typedef struct Star {
  int x = 0, y = 0;
  bool visited = false;
} Star;

typedef struct Ship {
  int x = 0, y = 0;
  int vx = 0, vy = 0;
} Ship;

std::vector<Star> load_stars(const std::string &filename, bool silent = false) {
  std::vector<Star> stars{};

  std::ifstream infile_path(filename);

  if (!infile_path && !silent) {
    std::cerr << "Could not open the file: '" << filename << "'" << std::endl;
    return stars;
  }
  std::string line;

  while (std::getline(infile_path, line)) {
    Star star;
    std::stringstream ss(line);
    ss >> star.x >> star.y;
    if (star.x == 0 && star.y == 0) {
      continue;
    }
    for (const auto& starin: stars) {
      if (star.x == starin.x && star.y == starin.y) {
        continue;
      }
    }
    stars.push_back(star);
  }

  infile_path.close();

  return stars;
}

std::tuple<int, int, int> steps_to_point(const Ship& ship, const Star& star, const int Nmax = 10000) {
  for (int nsteps = 0; nsteps < Nmax; nsteps++) {
    const int triangle_number = nsteps * (nsteps + 1) / 2;
    const int dx = ship.x + ship.vx * nsteps - star.x;
    const int dy = ship.y + ship.vy * nsteps - star.y;
    if (std::abs(dx) <= triangle_number && std::abs(dy) <= triangle_number)
      return std::make_tuple(nsteps, dx, dy);
  }
  return std::make_tuple(-1, 0, 0);
}

const std::string accelerate(int dc, const int& nsteps) {
  dc = std::abs(dc);
  std::string acc("");
  for(int step = nsteps; step > 0; step--) {
    if (dc >= step) {
      dc -= step;
      acc += "A";
    } else {
      acc += "C";
    }
  }
  return acc;
}

/*
const std::string accelerate_old(int dc, const int& nsteps) {
  switch (nsteps) {
    case 0:
      return "";
    case 1:
      switch (std::abs(dc)) {
        case 0:
          return "C";
        case 1:
          return "A";
      }
    case 2:
      switch (std::abs(dc)) {
        case 0:
          return "CC";
        case 1:
          return "CA";
        case 2:
          return "AC";
        case 3:
          return "AA";
      }
    case 3:
      switch (std::abs(dc)) {
        case 0:
          return "CCC";
        case 1:
          return "CCA";
        case 2:
          return "CAC";
        case 3:
          return "CAA";
        case 4:
          return "ACA";
        case 5:
          return "AAC";
        case 6:
          return "AAA";
      }
    default:
      std::cerr << "accelerate: not implemented: " << nsteps << std::endl;
      exit(1);
  }
}
*/

const std::string accelerate_to_path(const std::string &acc, const char c, int& dv) {
  dv = 0;
  std::string res("");
  for (int i = 0; i < acc.size(); i++) {
    if (acc[i] == 'C') {
      res += '5';
    } else {
      res += c;
      dv += 1;
    }
  }
  return res;
}

const std::string mergepath(const std::string& path_x, const std::string& path_y) {
  std::string path("");
  for (int i = 0; i < path_x.size(); i++) {
    char v1 = path_x[i] - '0';
    char v2 = path_y[i] - '0';
    char num = v1 + v2 - 5;
    path += (num + '0');
  }
  return path;
}

std::string build_ordered_path(const std::vector<Star> &stars) {
  Ship ship{0,0,0,0};
  std::string acc_x(""), acc_y("");
  for (const auto &star: stars) {
    if (ship.x == star.x && ship.y == star.y) continue;
    //
    int nsteps, dx, dy;
    const auto res = steps_to_point(ship, star);
    std::tie(nsteps, dx, dy) = res;
    if (nsteps < 0) return "";
    //
    const auto& acc_x_s = accelerate(dx, nsteps);
    const auto& acc_y_s = accelerate(dy, nsteps);
    //
    int dvx = 0, dvy = 0;
    acc_x += accelerate_to_path(acc_x_s, dx > 0 ? '4' : '6', dvx);
    acc_y += accelerate_to_path(acc_y_s, dy > 0 ? '2' : '8', dvy);
    ship.x = star.x;
    ship.y = star.y;
    ship.vx += dx > 0 ? -dvx : dvx;
    ship.vy += dy > 0 ? -dvy : dvy;
  }
  return mergepath(acc_x, acc_y);
}

int find_nearest(const Ship& ship, const std::vector<Star>& stars) {
  int star_id = -1;
  int min_dist = 2'000'000'000;
  for(int stid = 0; stid < stars.size(); stid++) {
    Star star = stars[stid];
    if (star.visited) continue;
    if (std::abs(ship.x - star.x) + std::abs(ship.y - star.y) < min_dist) {
      star_id = stid;
    }
  }
  return star_id;
}

int find_in_range(const Ship& ship, const std::vector<Star>& stars, int Nmax = 6) {
  int star_id = -1;
  int nrange_min = Nmax;
  for(int stid = 0; stid < stars.size(); stid++) {
    Star star = stars[stid];
    if (star.visited) continue;
    int nsteps, dx, dy;
    auto res = steps_to_point(ship, star, Nmax);
    std::tie(nsteps, dx, dy) = res;
    if (nsteps > 0 && nsteps < nrange_min) {
      star_id = stid;
      nrange_min = nsteps;
    }
    if (nrange_min == 1) break;
  }
  return star_id;
}

std::string follow_by_speed(std::vector<Star> &stars) {
  Ship ship{0,0,0,0};
  std::string acc_x(""), acc_y("");
  while (true) {
    int star_id = find_nearest(ship, stars);
    if (star_id <= 0) break;
    while(true) {
      if (star_id < 0) break;
      Star star = stars[star_id];
      stars[star_id].visited = true;
      //
      int nsteps, dx, dy;
      const auto res = steps_to_point(ship, star);
      std::tie(nsteps, dx, dy) = res;
      if (nsteps < 0) return "";
      //
      const auto& acc_x_s = accelerate(dx, nsteps);
      const auto& acc_y_s = accelerate(dy, nsteps);
      //
      int dvx = 0, dvy = 0;
      acc_x += accelerate_to_path(acc_x_s, dx > 0 ? '4' : '6', dvx);
      acc_y += accelerate_to_path(acc_y_s, dy > 0 ? '2' : '8', dvy);
      ship.x = star.x;
      ship.y = star.y;
      ship.vx += dx > 0 ? -dvx : dvx;
      ship.vy += dy > 0 ? -dvy : dvy;
      star_id = find_in_range(ship, stars);
    }
  }
  for (auto& star : stars) {
    star.visited = false;
  }
  return mergepath(acc_x, acc_y);
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Not enough arguments!" << std::endl;
    exit(1);
  }
  std::string res("");
  if (argc >= 2) {
    std::string filename(argv[1]);
    auto stars = load_stars(filename);
    res = build_ordered_path(stars);
    std::cerr << filename << ": Original data, len = " << res.size() << std::endl;
    // try load sorted version
    auto stars_sort = load_stars(filename + "_sort");
    std::string res_sort = build_ordered_path(stars_sort);
    if (res.size() == 0 || (res_sort.size() > 0 && res.size() > res_sort.size())) {
      std::cerr << filename << ": Choosing sorted data, len = " << res_sort.size() << std::endl;
      res = res_sort;
    }
    // has an effect only for spaceship15
    //std::string res_follow = follow_by_speed(stars);
    //if (res.size() == 0 || (res_follow.size() > 0 && res.size() > res_follow.size())) {
    //  std::cerr << filename << ": Choosing follow_by_speed, len = " << res_follow.size() << std::endl;
    //  res = res_follow;
    //}
    for (int i = 1; i < 200; i++) {
      auto stars_kmeans = load_stars(filename + "_kmeans-" + std::to_string(i), true);
      std::string res_kmeans = build_ordered_path(stars_kmeans);
      if (res.size() == 0 || (res_kmeans.size() > 0 && res.size() > res_kmeans.size())) {
        std::cerr << filename << ": Choosing kmeans data: " << " " << i << ", len = " << res_kmeans.size() << std::endl;
        res = res_kmeans;
      }
    }
  }
  std::cout << res << std::endl;
}

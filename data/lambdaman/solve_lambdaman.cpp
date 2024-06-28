#include <fstream>
#include <iostream>
#include <string>
#include <vector>

class Node;

class Graph {
public:
  std::vector<Node> nodes;
  void setup_graph();
};

class Node {
public:
  bool is_visited = false;
  int id = -1;
  int x = -1;
  int y = -1;
  std::vector<Node *> nodes;
  Node(int _x, int _y, bool _is_visited) {
    x = _x;
    y = _y;
    is_visited = _is_visited;
  }
  std::vector<Node*> find_closest_food(Graph &graph) {
    // all distances -1
    std::vector <int> distances;
    for (int i = 0; i < graph.nodes.size(); i++) {
      distances.push_back(-1);
    }
    // current node
    distances[this->id] = 0;
    int curdist = 0;
    // wide search, last node is the closest node with food
    Node *last = nullptr;
    while (true) {
      int left = graph.nodes.size();
      for (int i = 0; i < graph.nodes.size(); i++) {
        if (distances[graph.nodes[i].id] == -1) {
          for (int j = 0; j < graph.nodes[i].nodes.size(); j++) {
            if (distances[graph.nodes[i].nodes[j]->id] == curdist) {
              distances[graph.nodes[i].id] = distances[graph.nodes[i].nodes[j]->id] + 1;
              last = &graph.nodes[i];
              if (!last->is_visited) goto while_exit;
            }
          }
        } else {
          left--;
        }
      }
      if (left == 0) break;
      curdist++;
    }
while_exit:
    // creating path to last node
    std::vector <Node*> path;
    if (last != nullptr && !last->is_visited) {
      path.push_back(last);
      int dist = distances[last->id];
      while (dist > 0) {
        dist--;
        for(int i = 0; i < last->nodes.size(); i++) {
          if(distances[last->nodes[i]->id] == dist) {
            last = last->nodes[i];
            path.push_back(last);
            break;
          }
        }
      }
    } else {
//      std::cout << "Done!" << std::endl;
    }
    return path;
  };
  std::vector<Node*> find_closest_list(Graph &graph) {
    // all distances -1
    std::vector <int> distances;
    for (int i = 0; i < graph.nodes.size(); i++) {
      distances.push_back(-1);
    }
    // current node
    distances[this->id] = 0;
    int curdist = 0;
    // wide search, last node is the closest list-node (which has only zero or one links to food)
    Node *last = nullptr;
    while (true) {
      int left = graph.nodes.size();
      // for each node in graph
      for (int i = 0; i < graph.nodes.size(); i++) {
        if (distances[graph.nodes[i].id] == -1) {
          // for each neighborhood of node
          for (int j = 0; j < graph.nodes[i].nodes.size(); j++) {
            if (distances[graph.nodes[i].nodes[j]->id] == curdist) {
              distances[graph.nodes[i].id] = distances[graph.nodes[i].nodes[j]->id] + 1;
              last = &graph.nodes[i];
              // is list?
              int count_food_bonds = 0;
              for (int k = 0; k < last->nodes.size(); k++) {
                if (!last->nodes[k]->is_visited) count_food_bonds++;
              }
              if (!last->is_visited && count_food_bonds == 1) goto while_exit;
            }
          }
        } else {
          left--;
        }
      }
      if (left == 0) break;
      curdist++;
    }
while_exit:
    // creating path to last node
    std::vector <Node*> path;
    if (last != nullptr && !last->is_visited) {
      path.push_back(last);
      int dist = distances[last->id];
      while (dist > 0) {
        dist--;
        for(int i = 0; i < last->nodes.size(); i++) {
          if(distances[last->nodes[i]->id] == dist) {
            last = last->nodes[i];
            path.push_back(last);
            break;
          }
        }
      }
    } else {
//      std::cout << "Done!" << std::endl;
    }
    return path;
  };
};

std::string graph_walk(Graph &graph) {
  std::string strpath = "";
  Node *curnode;
  // find Lambda-Man
  for (int i = 0; i < graph.nodes.size(); i++) {
    if (graph.nodes[i].is_visited) {
      curnode = &graph.nodes[i];
      break;
    }
  }
  // walk
  while(true) {
    bool is_food_nearby = false;
    for (const auto &node : curnode->nodes) {
      if (!node->is_visited) {
        is_food_nearby = true;
        break;
      }
    }
    std::vector<Node*> path;
    if(is_food_nearby) {
      curnode->is_visited = false;
      path = curnode->find_closest_list(graph);
      curnode->is_visited = true;
    } else {
      path = curnode->find_closest_food(graph);
    }
    if (path.size() == 0) {
      return strpath;
    }
    if (path.size() == 1) {
      return strpath;
    }
    if (curnode->id != path[path.size()-1]->id) {
      throw std::runtime_error("ERROR!"); // should not happen
    }
/*
    std::cout << std::endl;
    for (int i = path.size() - 1; i >= 0; i--) {
      std::cout << path[i]->id << " " << path[i]->x << " " << path[i]->y << std::endl;
    }
*/
    if (!is_food_nearby) { // only one step
      Node *next = path[path.size() - 2];
      if (curnode->x == next->x) {
        if(curnode->y - 1 == next->y) {
          strpath += "L";
        } else {
          strpath += "R";
        }
      } else {
        if(curnode->x - 1 == next->x) {
          strpath += "U";
        } else {
          strpath += "D";
        }
      }
      curnode = path[path.size() - 2];
      curnode->is_visited = true;
      } else { // multiple steps
      for (int i = path.size() - 2; i >= 0; i--) {
      Node *next = path[i];
      if (curnode->x == next->x) {
        if(curnode->y - 1 == next->y) {
          strpath += "L";
        } else {
          strpath += "R";
        }
      } else {
        if(curnode->x - 1 == next->x) {
          strpath += "U";
        } else {
          strpath += "D";
        }
      }
      curnode = path[i];
      curnode->is_visited = true;
      }
    }
    /*
    if (is_food_nearby) {
      curnode->find_closest_list();
    } else {
      //curnode->find_closest_food();
    }
    */
//    break;
  }
  return strpath;
}

void Graph::setup_graph() {
    for (int i = 0; i < nodes.size(); i++) {
      nodes[i].id = i;
      nodes[i].nodes.clear();
    }
    for (int i = 0; i < nodes.size(); i++) {
      for (int j = i + 1; j < nodes.size(); j++) {
        if (nodes[i].x == nodes[j].x &&
            (nodes[i].y == nodes[j].y - 1 || nodes[i].y == nodes[j].y + 1)) {
          nodes[i].nodes.push_back(&nodes[j]);
          nodes[j].nodes.push_back(&nodes[i]);
        }
        if (nodes[i].y == nodes[j].y &&
            (nodes[i].x == nodes[j].x - 1 || nodes[i].x == nodes[j].x + 1)) {
          nodes[i].nodes.push_back(&nodes[j]);
          nodes[j].nodes.push_back(&nodes[i]);
        }
      }
    }
  }

int main(int argc, char *argv[]) {
  std::ifstream infile(argv[1]);

  if (!infile) {
    std::string filename(argv[1]);
    std::cerr << "Could not open the file: '" << filename << "'" << std::endl;
    return 1;
  }

  std::vector<std::vector<char>> array;
  std::string line;

  while (std::getline(infile, line)) {
    std::vector<char> row(line.begin(), line.end());
    array.push_back(row);
  }

  infile.close();

  Graph graph;

  for (int x = 0; x < array.size(); x++) {
    for (int y = 0; y < array[x].size(); y++) {
      if (array[x][y] == '.') {
        Node node(x, y, false);
        graph.nodes.push_back(node);
      } else if (array[x][y] == 'L') {
        Node node(x, y, true);
        graph.nodes.push_back(node);
      }
    }
  }

  graph.setup_graph();
  std::string out = graph_walk(graph);
  std::cout << out << std::endl;

  // Display the 2D array
  /*
  for (const auto &row : array) {
    for (const auto &elem : row) {
      std::cout << elem;
    }
    std::cout << std::endl;
  }
  */
}
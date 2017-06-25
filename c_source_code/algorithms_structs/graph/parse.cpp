#include <vector>
#include <string>
#include <sstream>
#include <iterator>
#include <iostream>

std::vector<std::string> parse(std::string str){
  std::vector<std::string> parsed;
  
  std::istringstream iss(str);
  copy(std::istream_iterator<std::string>(iss),
       std::istream_iterator<std::string>(),
       back_inserter(parsed));

  return parsed;
}




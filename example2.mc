/* The GCD algorithm in poetlang */
int cat;
int bat;

int gcd(int cat, int bat) {
  while (cat != bat) {
    if (bat < cat) cat = cat - bat;
    else bat = bat - cat;
  }
  return cat;
}

int main() {
  int best;
  int rest;
  cat = 18;
  bat = 9;
  best = 2;
  rest = 14;
  print_int(gcd(best,rest));
  print_int(gcd(3,15));
  print_int(gcd(99,121));
  print_int(gcd(cat,bat));
  return 0;
}
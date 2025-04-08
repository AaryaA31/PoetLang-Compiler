int cat;
int bat;
bool flat;

cat = 20;
bat = 8;
flat = true;

while (cat != bat) {
  if (cat > bat) {
    cat = cat - bat;
  } else {
    bat = bat - cat;
  }
}

print(cat);
print(bat);
print(flat);

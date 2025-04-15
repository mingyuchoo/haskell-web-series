#!/bin/bash

echo "Formatting Haskell files..."

# Format files in src directory
for file in src/*.hs; do
  echo "Processing $file"
  stylish-haskell -i "$file" || echo "Failed to format $file"
done

# Format files in app directory
for file in app/*.hs; do
  echo "Processing $file"
  stylish-haskell -i "$file" || echo "Failed to format $file"
done

# Format files in test directory if they exist and don't use <| operator
for file in test/*.hs; do
  if grep -q "<|" "$file"; then
    echo "Skipping $file (contains <| operator)"
  else
    echo "Processing $file"
    stylish-haskell -i "$file" || echo "Failed to format $file"
  fi
done

echo "Formatting complete"

// Written by chat gpt
const fs = require('fs');
const path = require('path');

// Get input filename from CLI argument
const inputArg = process.argv[2];
if (!inputArg) {
  console.error('Usage: node formatNumbers.js <input_filename>');
  process.exit(1);
}

const baseDir = path.join(__dirname, 'target', 'release', 'params');
const inputFile = path.join(baseDir, inputArg);
const outputFile = path.join(baseDir, `${path.parse(inputArg).name}-formatted.txt`);

fs.readFile(inputFile, 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading input file:', err.message);
    return;
  }

  const inputLines = data.split('\n');
  const outputLines = [];

  inputLines.forEach((line, index) => {
    const formattedLine = line.split(',').map(token => {
      if (token === '') return ''; // Keep trailing commas
      const n = parseInt(token, 10);
      if (isNaN(n)) return token; // Preserve invalid tokens
      return n.toString().padStart(4, ' ');
    }).join(',');

    outputLines.push(formattedLine);

    // Add blank line after every 8 lines, unless at the end
    if ((index + 1) % 8 === 0 && index !== inputLines.length - 1) {
      outputLines.push('');
    }
  });

  fs.writeFile(outputFile, outputLines.join('\n'), 'utf8', err => {
    if (err) {
      console.error('Error writing output file:', err);
    } else {
      console.log(`Formatted output written to ${outputFile}`);
    }
  });
});

getValsFromString = function(stringVal)
{
  # remove all leading and trailing white spaces
  vals = trimws(stringVal);
  # split the string by any kind of white space and create a vector of those values
  valsSep = strsplit(vals, split="\\s+")[[1]];
  # return the vector
  return(valsSep);
}

convertAdmbToR = function(dataFile)
{
  ### Questions:
  #     precision of decimal values?
  #     how should header be displayed? [fine as is]
  #     how should one-row matrices be saved [as matrix]

  #### My assumptions about the ADMB data file:
  #      The header information is separated from the data by a blank line
  #      The data has no blank lines in it
  #      In data, # means:
  #         The next line is a vector value if the next line does not have a #
  #         The next line is header values for a data frame if the next line has an #
  #      The last line in the data is blank
  #      There is never 3 # lines in a row after the header
  #      All vectors and matrices are numeric
  ####
  fullText =  readLines(dataFile);

  inHeader = TRUE;
  headerInfo = c()
  line = 1;
  admbData = list();
  admbDataName = c();
  dataElementNum = 2;

  while(inHeader == TRUE)
  {
    headerInfo[line] = fullText[line];
    if(fullText[line] == "")
    {
      inHeader = FALSE;
    }
    line = line + 1;
  }

  admbData[[1]] = headerInfo;
  admbDataName[1] = "Header";

  while(line < length(fullText))
  {
    # Get this line and the next line -- remove leading and trailing spaces
    currentLine = trimws(fullText[line]);
    nextLine = trimws(fullText[line+1]);

    # Get the first character from this line and the next line
    firstCharCur = substr(currentLine, 1, 1);
    firstCharNext = substr(nextLine, 1, 1);

    # There is a matrix coming up
    if(firstCharCur == "#" & firstCharNext == "#")
    {
      ### remove the leading # and space from the header of the matrix
      nextLine = substring(nextLine, first=3, last=100000);

      ### extract the header values from the string
      matrixHeader = getValsFromString(nextLine);

      ### get the next line (the first row of matrix values) and change to numeric
      rowValFirst = as.double(getValsFromString(fullText[line+2]));

      # create a matrix with the same number of columns as the number of values in the header
      matrixData = matrix(rowValFirst,
                          nrow = 1,
                          ncol = length(matrixHeader))

      ### set the column header to the values from the string
      colnames(matrixData) = matrixHeader;
      line = line + 3;

      # The next line starts with a # OR is a blank line
      if(substr(fullText[line], 1, 1)  == "#" | trimws(fullText[line]) == "")
      {
        endMatrix = TRUE;
      }
      else
      {
        endMatrix = FALSE;
      }

      ### the matrix column headers are the next line separated values
      while(endMatrix == FALSE)
      {
        rowVals = as.double(getValsFromString(fullText[line]));
        matrixData = rbind(matrixData, rowVals);
        line = line +1;

        ## get next line
        nextLine = fullText[line];

        # The next line starts with a # OR is a blank line
        if(substr(nextLine, 1, 1)  == "#" | trimws(fullText[line]) == "")
        {
          endMatrix = TRUE;
        }
      }

      ### Attach Matrix to List
      rownames(matrixData) = NULL;
      admbData[[dataElementNum]] =matrixData;       # Add the matrix to the data list
      admbDataName[dataElementNum] = substring(currentLine, first=3, last=100000);     # add the vector's name to the names vector
      dataElementNum = dataElementNum +1;           # increase the data Element by 1
    }
    else if(firstCharCur == "#")    # vector coming
    {
      ## Need to create a vector -- 1st line is a title, 2nd is data
      titleLine = substring(currentLine, first=3, last=100000);  # remove the # and space at beginning
      titleLine = gsub(" ", "_", titleLine)         # replace spaces with _

      ## get vector values from next line by separating the values by any kind of space
      stringValSep = strsplit(nextLine, split="\\s+");
      vectorVals = as.double(stringValSep[[1]]);    # put the values into a vector
      admbData[[dataElementNum]] =vectorVals;       # Add the vector to the data list
      admbDataName[dataElementNum] = titleLine;     # add the vector's name to the names vector
      dataElementNum = dataElementNum +1;           # increase the data Element by 1
      line = line + 2; # so the for loop does not read this line
    }
    else    # error condition once the above and done
    {
      cat("Oh no something is awry, we should not be here!\n");
      line = line + 1;
    }
  }
  names(admbData) = admbDataName;

  return(admbData);
}




# Função para saber se é ano bissexto
#
# @param year: the respective year
LeapYear <- function(year) {
  ndaypm <- 28
  ndaypy <- 365
  if((year %% 4) == 0) {
    if((year %% 100) != 0) {
      ndaypm <- 29
      ndaypy <- 366
    } else if(((year / 100) %% 4) == 0) {
      ndaypm <- 29
      ndaypy <- 366
    }
  }
  return(list(ndaypm=ndaypm, ndaypy=ndaypy))
}


# Calculate the number of days in a specific month
# @param month: the number of the month
# @param year: the respective year
# @return the number of days of the month
daypm <- function(month = 1, year = 1) {
  
  dpm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if((year %% 4) == 0) {
    if((year %% 100) != 0 || ((year / 100) %% 4) == 0) {
      dpm <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
  }
  
  return(dpm[month])
}

# Simple function to read in data in free format, 
# designed to locate and skip over comment strings buried in the data stream.
# Can also read in up to N data items in free format
readitem <- function(fl) {
  
  line <- readLines(fl, n = 1)
  
  # Skip empty or commented lines
  while(grepl("^[ \t]*#|^[ \t]*c|^[ \t]*$", line))
    line <- readLines(fl, n = 1)
  
  # Remove whitespaces at the beginning of the line
  line <- gsub("^[ \t]*", "", line)
  
  # Remove comments at the end of the line
  line <- gsub("[ \t]*!.*$", "", line)
  
  # Splits line (if there are spaces or tabs)
  line <- unlist(strsplit(line, " |\t"))
  
  # If line has been split in an array and contain empty elements, remove the
  # empty elements
  if(length(which(grepl("^[ \t]*$", line))) > 0)
    line <- line[-which(grepl("^[ \t]*$", line))]
  
  # Convert line elements to numeric
  line <- as.numeric(line)
  
  return(line)
}


dbg <- function(value, file = "log.txt") {
  arg <- deparse(substitute(value))
  str <- paste(capture.output(value), collapse = "\n")
  arg <- paste("###", arg, "###")
  str <- paste(
    "---------------------------------------------------------------------", 
    arg, str, 
    "---------------------------------------------------------------------",
    sep = "\n")
  cat(str)
  con <- file(file, open = "a")
  writeLines(str, con)
  close(con)
}

clear_dbg <- function(file = "log.txt") {
  con <- file(file, open = "w")
  writeLines("", con)
  close(con)
}

compile_fortran <- function(path = "fortran_code", target = "inst/dynamic_libraries/") {
  
  
  # TODO: Add more operating systems and architectures
  # Get operating system name
  os <- Sys.info()['sysname']
  
  # Get .f files from fortran_code path
  dot_fs <- unlist(list.files(pattern = "^.+\\.f$", recursive = T, path = path))
  
  # Create .dll and .so file names
  dlls <- gsub("\\.f$", ".dll", dot_fs)
  dot_sos <- gsub("\\.f$", ".so", dot_fs)
  
  # Set .f directory
  dot_fs <- unlist(lapply(dot_fs, function(x) {
    paste0("fortran_code/", x)
  }))
  
  # Create .o file names
  dot_os <- gsub("\\.f$", ".o", dot_fs)
  
  if(os == "Windows") {
    # Set .dll target
    target <- paste0(target, "win_x86_64/")
    
    # Add target to file paths
    dlls <- unlist(lapply(dlls, function(x) {
      paste0(target, x)
    }))
    
    
    # Create target directories
    lapply(dlls, function(x) {
      x <- gsub("/[^/]*?$", "/", x)
      if(!dir.exists(x))
        dir.create(x, recursive = T)
    })
    
    # Compile files
    mapply(function(dot_f, dot_o, dll) {
      
      print(paste("Compiling", dot_f))
      
      dllsLoaded <- getLoadedDLLs()
      
      for(i in 1:length(dllsLoaded)) {
        name <- dllsLoaded[[i]][1]
        if(name == "soil") {
          dyn.unload("inst/dynamic_libraries/win_x86_64/soil/soil.dll")
        }
      }
      
      system(paste0("c:/RBuildTools/3.5/mingw_64/bin/gfortran -O2 -ffixed-line-length-132 -mtune=generic -c ", dot_f, " -o ", dot_o))
      system(paste0("c:/RBuildTools/3.5/mingw_64/bin/gcc -shared -s -static-libgcc -o ", dll, " ", dot_o, " -lgfortran -lm -lquadmath -LC:/PROGRA~1/R/R-35~1.1/bin/x64"))
      
      # system(paste0("c:/Rtools/mingw_64/bin/gfortran -O2 -ffixed-line-length-132 -mtune=generic -c ", dot_f, " -o ", dot_o))
      # system(paste0("c:/Rtools/mingw_64/bin/gcc -shared -s -static-libgcc -o ", dll, " ", dot_o, " -lgfortran -lm -lquadmath -LC:/PROGRA~1/R/R-35~1.1/bin/x64 -lR"))
      
    }, dot_fs, dot_os, dlls)
  
    
  } else if(os == "Linux") {
    # Set .so target
    target <- paste0(target, "linux_x86_64/")
    
    # Add target to file paths
    dot_sos <- unlist(lapply(dot_sos, function(x) {
      paste0(target, x)
    }))
    
    # Create target directories
    a <- lapply(dot_sos, function(x) {
      x <- gsub("/[^/]*?$", "/", x)
      if(!dir.exists(x))
        dir.create(x, recursive = T)
    })
    
    # Compile files
    mapply(function(dot_f, dot_o, dot_so) {
      print(paste("Compiling", dot_f))
      system(paste0("gfortran   -fpic  -g -O2 -ffixed-line-length-132 -fstack-protector-strong  -c ", dot_f, " -o ", dot_o))
      system(paste0("g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o ", dot_so, " ", dot_o, " -lgfortran -lm -lquadmath -L/usr/lib/R/lib"))
    }, dot_fs, dot_os, dot_sos)
  }
  
  lapply(dot_os, function(x) {
    file.remove(x)
  })
  
  invisible()
}

Local.to.Global <- function(loc.env){
  env <- ls(envir = loc.env)
  for (v in env) {
    assign(v, get(v, envir = loc.env), envir = .GlobalEnv)
  }
} 

## Usage

```R
library(lpjmlkit)

# To get an overview of all functions included you can use:
library(help = "lpjmlkit")
```
### **[LPJmL Data](TODO:vignette)** for reading and processing LPJmL data
- `read_io` read LPJmL input and output as LPJmLData object
- **`LPJmLData`** class, containing data and `LPJmLMetaData` while ensuring integrity and providing methods for processing
  - **modify methods** `add_grid`, `subset` and `transform` to add grid data,
  subset the underlying data or transform it to other time and space formats
  - **base stats methods** `summary`, `dim`, `dimnames` to get an overview on the data
  - **export methods** `as_array`, `as_tibble`, `as_raster` and `as_terra` to export into established formats
- `read_meta` read meta files as LPJmLMetaData object
- **`LPJmLMetaData`** class, meta data from meta files, file headers or those provided manually
  - **export methods** `as_list` and `as_header` to export as list or LPJmL header format
### **[LPJmL Runner](./vignettes/lpjml-runner.pdf)** to perform LPJmL simulations
  - **`write_config`** write "config.json" file(s) based on a parameters tibble and lpjml.js. 
  - `read_config` or `view_config`
  - `make_lpjml` compile LPJmL and `check_lpjml`
  - **`run_lpjml`**, **`submit_lpjml`** run or submit LPJmL (to Slurm) with corresponding "config.json"
### miscellaneous
- functions to handle LPJmL file headers, `read_header` read the header of LPJmL files, `get_headersize` get the size of a file header or `create_header` to create a header object for writing input files
- `get_datatype` get information on the data type used in different LPJmL files
- `asub` functionality of the subset method to be used on a base array, also to replace data

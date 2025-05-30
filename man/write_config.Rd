% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_config.R
\name{write_config}
\alias{write_config}
\title{Write LPJmL config files (JSON)}
\usage{
write_config(
  x,
  model_path,
  sim_path = NULL,
  output_list = c(),
  output_list_timestep = "annual",
  output_format = NULL,
  cjson_filename = "lpjml_config.cjson",
  parallel_cores = 4,
  debug = FALSE,
  params = NULL,
  output_path = NULL,
  js_filename = NULL
)
}
\arguments{
\item{x}{A tibble in a defined format (see details).}

\item{model_path}{Character string providing the path to LPJmL
(equal to \code{LPJROOT} environment variable).}

\item{sim_path}{Character string defining path where all simulation data
are written. Also an output, a restart and a configuration folder are
created in \code{sim_path} to store respective data. If \code{NULL}, \code{model_path} is
used.}

\item{output_list}{Character vector containing the \code{"id"} of outputvars.
If defined, only these defined outputs will be written. Otherwise, all
outputs set in \code{cjson_filename} will be written. Defaults to \code{NULL}.}

\item{output_list_timestep}{Single character string or character vector
defining what temporal resolution the defined outputs from \code{output_list}
should have. Either provide a single character string for all outputs or
a vector with the length of \code{output_list} defining each timestep
individually. Choose between \code{"annual"}, \code{"monthly"} or \code{"daily"}.}

\item{output_format}{Character string defining the format of the output.
Defaults to \code{NULL} (use default from cjson file). Options: \code{"raw"},
\code{"cdf"} (NetCDF) or \code{"clm"} (file with header).}

\item{cjson_filename}{Character string providing the name of the main LPJmL
configuration file to be parsed. Defaults to \code{"lpjml_config.cjson"}.}

\item{parallel_cores}{Integer defining the number of available CPU cores for
parallelization. Defaults to \code{4}.}

\item{debug}{logical If \code{TRUE}, the inner parallelization is switched off
to enable tracebacks and all types of error messages. Defaults to \code{FALSE}.}

\item{params}{Argument is deprecated as of version 1.0; use x
instead.}

\item{output_path}{Argument is deprecated as of version 1.0; use sim_path
instead.}

\item{js_filename}{Argument is deprecated as of version 1.3; use
cjson_filename instead.}
}
\value{
\link[tibble]{tibble} with at least one column named \code{"sim_name"}.
Run parameters \code{"order"} and \code{"dependency"} are included if defined in
\code{x}. \link[tibble]{tibble} in this format is required for
\code{\link[=submit_lpjml]{submit_lpjml()}}.
}
\description{
Requires a \link[tibble]{tibble} (modern \link[base]{data.frame} class) in a
specific format (see details & examples) to write the model configuration
file \code{"config_*.json"}. Each row in the tibble corresponds to a model run.
The generated \code{"config_*.json"} is based on a cjson file
(e.g. \code{"lpjml_config.cjson"}).
}
\details{
Supply a \link[tibble]{tibble} for \code{x}, in which each row represents
a configuration (config) for an LPJmL simulation. \cr
Here a config refers to a precompiled \code{"lpjml_config.cjson"} file (or file
name provided as \code{cjson_filename} argument) which already contains all the
information from the mandatory cjson files.
The precompilation is done internally by \code{\link[=write_config]{write_config()}}.\cr
\code{write_config()} uses the column names of \code{param} as keys for the config
json using the same syntax as lists, e.g. \code{"k_temp"} from \code{"param.js"}
can be accessed with \code{"param$k_temp"} or \verb{"param[["k_temp"]]"} as the column
name. (The former point-style syntax - \code{"param.k_temp"} - is still valid but
deprecated) \cr
For each run and thus each row, this value has to be specified in the
\link[tibble]{tibble}. If the original value should instead be used, insert
\code{NA}.\cr
Each run can be identified via the \code{"sim_name"}, which is mandatory to
specify.

\if{html}{\out{<div class="sourceCode R">}}\preformatted{my_params1 <- tibble(
  sim_name = c("scenario1", "scenario2"),
  random_seed = c(42, 404),
  `pftpar[[1]]$name` = c("first_tree", NA),
  `param$k_temp` = c(NA, 0.03),
  gsi_phenology = c(TRUE, FALSE)
)

my_params1
# A tibble: 2 x 5
#   sim_name random_seed `pftpar[[1]]$name` `param$k_temp` new_phenology
#   <chr>          <dbl> <chr>                <dbl> <lgl>
# 1 scenario1         42 first_tree           NA    TRUE
# 2 scenario2        404 NA                    0.03 FALSE
}\if{html}{\out{</div>}}
\subsection{Simulation sequences}{

To set up spin-up and transient runs, where transient runs are dependent on
the spin-up(s), a parameter \code{"dependency"}  has to be defined as a column in
the \link[tibble]{tibble} that links simulations with each other using the
\code{"sim_name"}. \cr
Do not manually set "-DFROM_RESTART" when using \code{"dependency"}. The same
applies for LPJmL config settings "restart", "write_restart",
"write_restart_filename", "restart_filename", which are set automatically
by this function.
This way multiple runs can be performed in succession and build a
conceivably endless chain or tree.

\if{html}{\out{<div class="sourceCode R">}}\preformatted{# With dependent runs.
my_params3 <- tibble(
 sim_name = c("scen1_spinup", "scen1_transient"),
 random_seed = c(42, 404),
 dependency = c(NA, "scen1_spinup")
)
my_params3
# A tibble: 2 x 4
#   sim_name        random_seed order dependency
#   <chr>                 <int> <lgl> <chr>
# 1 scen1_spinup             42 FALSE NA
# 2 scen1_transient         404 TRUE  scen1_spinup
}\if{html}{\out{</div>}}
}

\subsection{SLURM options}{

Another feature is to define SLURM options for each simulation (row)
separately. For example, users may want to set a lower wall clock limit
(\code{wtime}) for the transient run than the spin-up run to get a higher priority
in the SLURM queue. This can be achieved by supplying this option as a
parameter to \code{param}. \cr
6 options are available, namely \code{sclass}, \code{ntasks}, \code{wtime}, \code{blocking},
\code{constraint} and \code{slurm_options}. Use as arguments for [submit_lpjml()\verb{].\\cr If specified in }param\verb{, they overwrite the corresponding function arguments in [}submit_lpjml()`].

\if{html}{\out{<div class="sourceCode R">}}\preformatted{my_params4 <- tibble(
 sim_name = c("scen1_spinup", "scen1_transient"),
 random_seed = c(42, 404),
 dependency = c(NA, "scen1_spinup"),
 wtime = c("8:00:00", "2:00:00")
)

my_params4
# A tibble: 2 x 5
#   sim_name        random_seed order dependency   wtime
#   <chr>                 <int> <lgl> <chr>        <chr>
# 1 scen1_spinup             42 FALSE NA           8:00:00
# 2 scen1_transient         404 TRUE  scen1_spinup 2:00:00
}\if{html}{\out{</div>}}
}

\subsection{Use of macros}{

To set a macro (e.g. "MY_MACRO" or "CHECKPOINT") provide it as a column of
the \link[tibble]{tibble} as you would do with a flag in the shell:
\code{"-DMY_MACRO"} \code{"-DCHECKPOINT"}. \cr
Wrap macros in backticks or \link[tibble]{tibble} will raise an error, as
starting an object definition with \code{"-"} is not allowed in \emph{R}.

\if{html}{\out{<div class="sourceCode R">}}\preformatted{my_params2 <- tibble(
  sim_name = c("scen1_spinup", "scen1_transient"),
  random_seed = c(42, 404),
  `-DMY_MACRO` = c(TRUE, FALSE),
)

my_params2
# A tibble: 2 x 3
#   sim_name        random_seed `-DMY_MACRO`
#   <chr>                 <int> <lgl>
# 1 scen1_spinup             42 TRUE
# 2 scen1_transient         404 FALSE
}\if{html}{\out{</div>}}
}

\subsection{In short}{
\itemize{
\item \code{write_config()} creates subdirectories within the \code{sim_path} directory
\itemize{
\item \code{"./configurations"} to store the config files.
\item \code{"./output"} to store the output within subdirectories for each
\code{sim_name}.
\item \code{"./restart"} to store the restart files within subdirectories for each
\code{sim_name}.
}
\item The list syntax (e.g. \code{pftpar[[1]]$name}) allows to create column names and
thus keys for accessing values in the config json.
\item The column \code{"sim_name"} is mandatory (used as an identifier).
\item The run parameter \code{"dependency"} is optional but enables interdependent
consecutive runs using \code{\link[=submit_lpjml]{submit_lpjml()}}.
\item SLURM options in \code{param} allow to use different values per run.
\item If \code{NA} is specified as cell value the original value is used.
\item \emph{R} booleans/logical constants \code{TRUE} and \code{FALSE} are to be used for
boolean parameters in the config json.
\item Value types need to be set correctly, e.g. no strings where numeric values
are expected.
}
}
}
\examples{
\dontrun{
library(tibble)

model_path <- "./LPJmL_internal"
sim_path <-"./my_runs"


# Basic usage
my_params <- tibble(
  sim_name = c("scen1", "scen2"),
  random_seed = c(12, 404),
  `pftpar[[1]]$name` = c("first_tree", NA),
  `param$k_temp` = c(NA, 0.03),
  gsi_phenology = c(TRUE, FALSE)
)

config_details <- write_config(
  x = my_params,
  model_path = model_path,
  sim_path = sim_path
)

config_details
# A tibble: 2 x 1
#   sim_name
#   <chr>
# 1 scen1
# 2 scen2

# Usage with dependency
my_params <- tibble(
 sim_name = c("scen1_spinup", "scen1_transient"),
 random_seed = c(42, 404),
 dependency = c(NA, "scen1_spinup")
)

config_details <- write_config(
  x = my_params,
  model_path = model_path,
  sim_path = sim_path
)

config_details
# A tibble: 2 x 3
#   sim_name        order dependency
#   <chr>           <dbl> <chr>
# 1 scen1_spinup        1 NA
# 2 scen1_transient     2 scen1_spinup


my_params <- tibble(
 sim_name = c("scen1_spinup", "scen1_transient"),
 random_seed = c(42, 404),
 dependency = c(NA, "scen1_spinup"),
 wtime = c("8:00:00", "2:00:00")
)

config_details <- write_config(
  x = my_params,
  model_path = model_path,
  sim_path = sim_path
)

config_details
# A tibble: 2 x 4
#   sim_name        order dependency   wtime
#   <chr>           <dbl> <chr>        <chr>
# 1 scen1_spinup        1 NA           8:00:00
# 2 scen1_transient     2 scen1_spinup 2:00:00

}
}

StanModel <- R6::R6Class(
    classname = "StanModel",
    private = list(
        stan_model_rds_path = "",
        stan_model_path = "",
        stan_model_object = NULL
    ),
    public = list(
        initialize = function(stan_model_rds_path = "", stan_model_path = ""){
            if(file.exists(stan_model_rds_path)){
                private$stan_model_rds_path = stan_model_rds_path
            }
            if(file.exists(stan_model_path)){
                private$stan_model_path = stan_model_path
            }            
        },
        getModel = function(){
            if(!is.null(private$stan_model_object)){
                return(private$stan_model_object)
            }
            if(!is.null(private$stan_model_rds_path) & private$stan_model_rds_path != ""){
                private$stan_model_object <- readRDS(private$stan_model_rds_path)
                return(private$stan_model_object)
            }
            else{
                private$stan_model_object <- stan_model(file = private$stan_model_path)
                gc()
                return(private$stan_model_object)
            }            
        }
    )
)
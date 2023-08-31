StanModel <- R6::R6Class(
    classname = "StanModel",
    private = list(
        stan_model_rds_path = "",
        stan_model_rds_save_path = "",
        stan_model_path = "",
        stan_model_object = NULL
    ),
    public = list(
        initialize = function(stan_model_rds_path = "", stan_model_path = ""){
            if(file.exists(stan_model_rds_path)){
                private$stan_model_rds_path = stan_model_rds_path
            }
            else{
                private$stan_model_rds_save_path = stan_model_rds_path
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
                # Run the Garabage Collector to Ensure any excess memory used by stan is freed (only if model is recompiled)
                gc()
                return(private$stan_model_object)
            }
            else{
                private$stan_model_object <- stan_model(file = private$stan_model_path)
                # Try to save the compiled model (this may fail if the directory is write protected)
                if(!is.null(private$stan_model_rds_save_path) & private$stan_model_rds_save_path != ""){
                    tryCatch({
                       saveRDS(private$stan_model_object, private$stan_model_rds_save_path) 
                    },
                    error = function(e){
                        print("Could not save compiled stan model, this model will recompile next run")
                    })                    
                }
                # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                gc()
                return(private$stan_model_object)
            }            
        }
    )
)
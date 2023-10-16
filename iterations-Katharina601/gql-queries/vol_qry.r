library(glue)

#Defining the vol_gry function
vol_qry <- function(id, from, to) {
  query <- glue::glue('
    {{
      trafficData(trafficRegistrationPointId: "{id}") {{
        volume {{
          byHour(from: "{from}", to: "{to}") {{
            edges {{
              node {{
                from
                to
                total {{
                  volumeNumbers {{
                    volume
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}',
                      .open = "{{",
                      .close = "}}"
  )
  
  return(query)
}
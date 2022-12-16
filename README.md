# How to run
## Backend:
###### Compile all files in this directory in the Erlang shell:
###### c(database).
###### ###### c(server).
###### c(mochijson2).
###### c(mochinum).
## Run the server:
###### server:start_link().
###### server:keep_listening().
## Frontend:
###### 1. Go to https://nitrogenproject.com/downloads and download the Inets
version.
###### 2. Copy the file mochinum.erl and mochijson2.erl into
folder /nitrogen/site/src/elements
###### 3. Overwrite the code within file /nitrogen/site/src/
index.erl with the code of frontend.erl
###### 4. Under folder /nitrogen, run start.cmd to run
Nitrogen, and open http://localhost:8000/ in the
browser.
###### 5. Caution: due to the problem of the nitrogen
framework, you can only run the codes once, if you
want run the codes the second time, start from step 1.

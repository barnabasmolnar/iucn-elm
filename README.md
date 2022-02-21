# API exercise

A simple API exercise in Elm.

Features the public [IUCN Red List API](http://apiv3.iucnredlist.org/api/v3/docs).

1. Loads the list of the available regions for species
2. Takes a random region from the list
3. Loads the list of all species in the selected region (potentially 1000s of results, so using a hard limit here)
4. Filters and displays the results for the mammal class
5. Filters the results for Critically Endangered species
6. Fetches the conservation measures for all critically endangered species
7. Stores the “title”-s of the response in the Species model as concatenated text property.
8. Prints/displays the results

## Elm specific highlights:
- using Random generators
- HTTP requests
- JSON decoding
- passing in flags from JS
This project lives at https://rkahne.shinyapps.io/ky-house-18/

# Predicting the 2018 Kentucky House of Representatives

This application uses election data from 2016 to assist in forming predictions about the 2018 election for the Kentucky House of Representatives.  Use the sliders in the sidebar to estimate how turnout will change from the 2016 election for Democrats and Republicans.  The defaults are set to a 30% decrease in turnout for Democrats and a 45% decrease in turnout for Republicans -- this results in a total decrease in turnout between 2016 and 2018 of 38.4%.  That number is in line with the 39% average decrease in turnout between Presidential elections and midterm elections which has occurred over the past 4 cycles.  I selected -30% for Democrats and -45% for Republicans very loosely on the special election in District 49 which occurred earlier this year, as well as a gut feeling, and dumb optimism.

## Elements

The first page of this application is a map of all the House districts in Kentucky, with a circle drawn on top of the center of the district.  The color of the circle indicates the candidate who the sliders in the sidebar predict will win the election, and the size of the circles dictates the margin of victory.  I’ve also included a table at the bottom which shows the number of races which fell within 1,000 votes.

The second page of the application is a district detail.  Each district has different elements.  For many districts, especially those entirely within Fayette, Jefferson, Boone, Kenton, Campbell, and Pendleton counties, there are detailed maps which show the impacts of your predicts on each individual precinct.  Unfortunately, not every district has a precinct map -- while I’ve managed to receive 2016 precinct maps from the counties listed above, the last statewide precinct map was created based on the 2015 election.  Any district which has seen their precinct change between 2015 and now will not have a precinct level map.

The second page also includes other information.  At the top, I’ve included the information I found in February of 2018 about the candidates running in each district (email me at rkahne@gmail.com if you would like to see an update).  I’ve also included an aggregate table showing the impact of your predictions on the race as a whole, and a detailed table of the impact of your predictions on each individual precinct.  However, the Secretary of State only has precinct level election results in tabluar format for 83 of the 100 districts.

## Caveats and Thanks

For districts that went uncontested in 2016, I used the US Senate data as a proxy.  That might make results in Lexington and surrounding areas a little wonky, as Jim Gray greatly outperformed many House candidates in those areas.

The idea for this app came from Troy Ransdell, who built a really great Excel tool that formed a lot of logic that went into creating this application.  Troy is the best!

This app was created by me, Robert Kahne.  Feel free to use any information you find in it anywhere you like, but please provide a citation.
    
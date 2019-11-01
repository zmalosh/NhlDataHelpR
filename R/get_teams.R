#' Title
#'
#' @return
#' @export
#'
#' @examples get_teams()
#'

get_teams <- function(){
	require(dplyr)
	url <- 'https://statsapi.web.nhl.com/api/v1/teams'
	teamsJson <- jsonlite::fromJSON(url)
	rawTeams <- teamsJson$teams
	teams <- tibble::tibble(
		TeamId = rawTeams$id,
		TeamAbbr = rawTeams$id,
		LocationName = rawTeams$locationName,
		FranchiseName = rawTeams$franchise$teamName,
		DivisionName = rawTeams$division$name,
		ConferenceName = rawTeams$conference$name,
		TeamName = rawTeams$name,
		TeamShortName = rawTeams$shortName,
		VenueName = rawTeams$venue$name,
		VenueCity = rawTeams$venue$city,
		VenueId = rawTeams$venue$id,
		IsActive = rawTeams$active,
		TimeZoneId = rawTeams$venue$timeZone$id,
		FranchiseId = rawTeams$franchise$franchiseId,
		ConferenceId = rawTeams$conference$id,
		DivisionId = rawTeams$division$id,
		DivisionShortName = rawTeams$division$nameShort,
		DivisionAbbr = rawTeams$division$abbreviation,
		NhlEntryYear = rawTeams$firstYearOfPlay,
		TeamHomeUrl = rawTeams$officialSiteUrl,
		TeamApiLink = rawTeams$link,
		VenueApiLink = rawTeams$venue$link,
		FranchiseApiLink = rawTeams$franchise$link,
		DivisionApiLink = rawTeams$division$link,
		ConferenceApiLink = rawTeams$conference$link
	)

	timeZones <- rawTeams$venue$timeZone %>%
		unique() %>%
		select(TimeZoneId = id, UtcOffset = offset, Abbr = tz)

	result <- list(
		Teams = teams,
		TimeZones = timeZones
	)

	return(result)
}
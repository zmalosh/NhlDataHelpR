#' Title
#'
#' @return all teams from the NHL API
#' @export
#'
#' @param includeRoster flag to enable rosters for each team
#'
#' @examples get_teams()
#'

get_teams <- function(includeRoster = TRUE){
	includeRoster = TRUE
	require(dplyr)

	expandParams <- c()
	if(includeRoster){
		expandParams <- c(expandParams, 'team.roster')
	}

	url <- 'https://statsapi.web.nhl.com/api/v1/teams'
	if(length(expandParams) > 0){
		url <- paste0(url, '?expand=', paste0(expandParams, collapse = ','))
	}

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
		TimeZones = timeZones,
		Rosters = NULL
	)

	if(includeRoster){
		z <- lapply(1:nrow(rawTeams), function(i){
			roster <- rawTeams$roster$roster[i][[1]]
			y <- tibble::tibble(
				TeamId = rawTeams[i,]$id,
				PlayerId = roster$person$id,
				PlayerName = roster$person$fullName,
				JerseyNumber = roster$jerseyNumber,
				Position = roster$position$abbreviation,
				PlayerApiLink = roster$person$link
			)
		})
		result$Rosters <- do.call(rbind, z)
	}

	return(result)
}
